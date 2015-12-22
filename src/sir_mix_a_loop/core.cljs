(ns sir-mix-a-loop.core
  (:require [cljs.core.async :refer [chan <! close!]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

; TODO: 
; * optimisation
;  sort the samples into a dictionary by tick number for faster access inside the loop

; constants
(def scheduler-look-ahead-time 0.4)
(def scheduler-poll-time 0.05)

; id for each channel
(defonce player-id (atom 0))

; *** Initialize audio system components *** ;

; check for AudioContext availability and set
(defonce actx (when (aget js/window "AudioContext") (js/window.AudioContext.)))

; flag for the user to test if audio is available
(defonce audio? (if actx true false))

(defonce sample-rate (if actx (.-sampleRate actx) nil))

; *** Scheduling *** ;

; uses a webworker to run ticks even on a backgrounded tab
(let [metronome-worker-js "self.onmessage=function(e){setTimeout(function(){postMessage(e.data);},e.data.interval);};console.log('Metronome worker loaded.');"
      worker-blob (js/Blob. (clj->js [metronome-worker-js]) {:type "application/javascript"})
      worker (js/Worker. (.createObjectURL js/URL worker-blob))
      call-id (atom 0)]

  (defn make-worker-listener [id callback]
    (fn [e]
      (when (= e.data.id id)
        (callback)
        true)))

  (defn schedule-tick [callback interval]
    (let [id (swap! call-id inc)
          listener-fn (make-worker-listener id callback)]
      (.addEventListener worker
                         "message"
                         (fn [e]
                           (when (listener-fn e) (.removeEventListener worker "message" listener-fn)))
                         false)
      (.postMessage worker (clj->js {:id id :interval interval}))))

  ; this emulates async's timeout but in background worker thread
  (defn timeout-worker [interval]
    (let [c (chan)]
      (schedule-tick (fn [] (close! c)) interval)
      c)))

; *** Audio computation *** ;

(defn get-audio-time []
  (.-currentTime actx))

(defn tick-length [bpm] (/ 60.0 bpm))

(defn tick-at-time [{{:keys [audio-clock-started tick-offset]} :timing
                     {:keys [bpm]} :pattern}
                   t]
  (+ (int (/ (- t audio-clock-started) (tick-length bpm))) tick-offset))

(defn time-at-tick [{{:keys [audio-clock-started tick-offset]} :timing
                     {:keys [bpm]} :pattern}
                    t]
  (+ (* (- t tick-offset) (tick-length bpm)) audio-clock-started))

(defn get-samples-in-tick-range [samples ticks-loop-length tick-range]
  (let [ticks-range tick-range]
    (let [result 
          (apply concat
                 (map
                   (fn [s]
                     (remove nil? (for [tick ticks-range]
                                    (when (= (s :tick) (mod tick ticks-loop-length))
                                      (assoc s :tick tick)))))
                   samples))]
      result)))

; *** Audio node management *** ;

(defn make-sample-player [{:keys [data] :as sample-definition}]
  ; HTML5 audio buffer source
  (let [native-array (= (type data) js/Float32Array)
        frame-count (if native-array (.-length data) (count data))
        buffer-source (.createBufferSource actx)
        buffer-object (.createBuffer actx 1 frame-count sample-rate)]
    ; raw js audio buffer for this channel to write to
    (let [buffer-dest (.getChannelData buffer-object 0)]
      ; if we have been passed a native typed array
      (if native-array
        ; do a fast memcpy instead
        (.set buffer-dest data)
        ; otherwise we have to copy it manually
        ; for each individual sample in the count
        (loop [s 0]
          (when (< s frame-count)
            ; mutate the audio buffer to add this sample value
            (aset buffer-dest s (nth data s))
            (recur (inc s))))))
    ; set the buffer source on our new objects
    (set! (.-buffer buffer-source) buffer-object)
    ; pass back the audio node thing that is playable
    buffer-source))

(defn make-onended-fn [node]
  (fn [ev] (set! (.-ended node) true)))

(defn schedule-nodes-in-time-range! [{audio-node-output :audio-node-output
                                     {:keys [samples ticks-length]} :pattern
                                     {:keys [audio-clock-started]} :timing  :as player :or {audio-node-output (.-destination actx)}}
                                    tick-range]
  (let [samples-between-ticks (get-samples-in-tick-range samples ticks-length tick-range)]
    (into #{} (for [s samples-between-ticks]
                (let [node (make-sample-player s)
                      audio-clock-time-to-trigger-node (time-at-tick player (:tick s))]
                  ; connect it to the audio context output
                  (.connect node audio-node-output)
                  ; set the ended flag when ended
                  (set! (.-onended node) (make-onended-fn node))
                  ; trigger it to play at the correct time
                  (if (< (- audio-clock-time-to-trigger-node (.-currentTime actx)) 0) (print "*** Audio sample underrun! ***"))
                  (.start node audio-clock-time-to-trigger-node)
                  {:node node :source-data s})))))

(defn remove-played-nodes [audio-nodes]
  ; return a list of nodes that have finished playing
  (into #{} (filter #(.-ended (:node %)) audio-nodes)))

(defn stop-all-audio-nodes! [audio-nodes]
  (doseq [{:keys [node]} audio-nodes]
    (when (not (.-ended node))
      ; tell the node to cut audio immediately
      (.stop node)
      ; set the 'ended' flag on the note so it gets cleaned up
      (set! (.-ended node) true))))

; *** External player functions *** ;

(defn playing? [player] (when (:next-tick-time player) true))
(defn paused? [player] (when (and (not (:next-tick-time player)) (:tick player)) true))

(defn make-loop-player
  "Create the datastructure for playing a loop of audio."
  []
  (let [player-id (swap! player-id inc)]
    {:id player-id
           :tick 0
           :audio-nodes #{}}))

(defn play!
  "Start a loop player."
  [player-atom]
  (let [{original-id :id tick :tick {:keys [audio-clock-started]} :timing} @player-atom
        audio-clock-now (get-audio-time)]
    ; if we're not already playing
    (when (not audio-clock-started)
      ; set our variables for player start time
      (swap! player-atom #(-> %
                              (assoc-in [:timing :audio-clock-started] audio-clock-now)
                              (assoc-in [:timing :audio-clock-last-checked] audio-clock-now)
                              (assoc-in [:timing :tick-offset] (or tick 0))))
      ; note spawning task in a go loop subthread
      (go-loop [previous-audio-nodes #{} previous-tick-range ()]
               ; context for each frame
               (let [{{:keys [audio-clock-last-checked
                              audio-clock-started]} :timing
                      audio-nodes :audio-nodes
                      id :id} @player-atom]
                 ; if the user has bailed this atom, abandon ship
                 (if (= id original-id)
                   ; keep looping until stop! or pause! have unset timing settings
                   (when audio-clock-started
                     (let [audio-clock-now (get-audio-time)
                           ; what tick the audio clock thinks we're playing right now
                           tick-now (tick-at-time @player-atom audio-clock-now)
                           ; play notes from last check (to a limit of 1 frame behind some CPU delay occurred)
                           audio-clock-from (max audio-clock-last-checked (- audio-clock-now scheduler-look-ahead-time))
                           ; play notes up to the next look-ahead time with a limit of 1 frame ahead of now
                           audio-clock-to (min (+ audio-clock-last-checked scheduler-look-ahead-time) (+ audio-clock-now scheduler-look-ahead-time))
                           ; work out the range of ticks we need to consider currently in our look-ahead mixing
                           tick-from (tick-at-time @player-atom audio-clock-from)
                           tick-to (tick-at-time @player-atom audio-clock-to)
                           tick-range (range tick-from tick-to)
                           ; schedule all of the audio nodes that need to play soon
                           ; idempotent but side effect of creating audio node objects
                           new-nodes-set (when (not (= tick-range previous-tick-range)) (schedule-nodes-in-time-range! @player-atom tick-range))
                           ; remove any audio nodes that have already finished playing
                           played-nodes-set (remove-played-nodes audio-nodes)
                           ; remove the nodes that have finished playing
                           remaining-nodes-removed-played-set (apply disj audio-nodes played-nodes-set)
                           ; add in the new nodes we just started
                           remaining-nodes-added-new-set (apply conj remaining-nodes-removed-played-set new-nodes-set)]
                       ; detect underruns
                       (if (not (= audio-clock-from audio-clock-last-checked))
                         (print "*** Audio underrun! ***"))
                       ; update the player atom itself with the changes we have made
                       (swap! player-atom #(-> %
                                               ; update the set of nodes that are currently playing
                                               (assoc :audio-nodes remaining-nodes-added-new-set)
                                               ; update the last checked time to where we checked to
                                               (assoc-in [:timing :audio-clock-last-checked] audio-clock-to)
                                               ; update the current tick for UIs etc.
                                               (assoc :tick tick-now)))
                       ; wait until the next elapsed look-ahead-time
                       (<! (timeout-worker (* scheduler-poll-time 1000)))
                       (recur remaining-nodes-added-new-set tick-range)))
                   ; cleanup playing audio nodes
                   (stop-all-audio-nodes! previous-audio-nodes))))))
  @player-atom)

(defn pause!
  "Pause a loop player."
  [player-atom]
  (when (@player-atom :timing)
    (swap! player-atom dissoc :timing)
    (stop-all-audio-nodes! (@player-atom :audio-nodes)))
  @player-atom)

(defn stop!
  "Stop a loop player."
  [player-atom]
  ; tell the old audio loop to exit
  (when (@player-atom :timing)
    ; stop the audio engine from playing
    (pause! player-atom))
  ; reset the tick to the start
  (swap! player-atom assoc :tick 0)
  @player-atom)

