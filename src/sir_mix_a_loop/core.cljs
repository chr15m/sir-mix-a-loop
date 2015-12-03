(ns sir-mix-a-loop.core)

; check for AudioContext availability and set
(defonce actx (when (aget js/window "AudioContext") (js/window.AudioContext.)))

; id for each channel
(defonce channel-id (atom 0))

; flag for the user to test if audio is available
(defonce audio? (if actx true false))

(defonce sample-rate (if actx (.-sampleRate actx) nil))

(defn get-audio-time []
  (.-currentTime actx))

(defn calculate-frame-count [length-in-ms sample-rate]
  (* (* length-in-ms 0.001) sample-rate))

; taken from pure-data
(defn mtof "Convert MIDI note to frequency (Hz)." [m]
  (cond
    (<= m -1500) 0
    (> m 1499) (mtof 1499)
    :else (* (js/Math.exp (* m .0577622650)) 8.17579891564)))

; taken from pure-data
(defn ftom "Convert frequency (Hz) to MIDI note." [f]
  (if (> f 0)
    (* (js/Math.log (* f .12231220585)) 17.3123405046)
    -1500))

; run through all samples for the given time and mix the samples there
; TODO: only examine the samples that actually intersect our range (pre-compute)
(defn sum-sample-buffers-at-time [samples t-seconds]
  (apply +
         (doall (for [{:keys [data start-seconds length-seconds volume pan sample-sample-rate] :or {volume 1.0 pan 0.0 sample-sample-rate sample-rate}} samples]
                  (if (and (> t-seconds start-seconds) (< t-seconds (+ start-seconds length-seconds)))
                    (* (aget data (Math.round (* (- t-seconds start-seconds) sample-sample-rate))) volume)
                    0)))))

; update a two channel audio buffer with a loop definition
; TODO: use clojure.data/diff to only mix the bits of the buffer that changed
(defn convert-loop-to-buffers [channel old-loop-definition loop-definition]
  (let [{:keys [length-in-ms samples id]} loop-definition
        frame-count (calculate-frame-count length-in-ms sample-rate)
        buffers (or (channel :buffers) (.createBuffer actx 2 frame-count sample-rate))]
    ; for left and right channels
    (doseq [c (range 2)]
      ; grab the raw js audio buffer for this channel to write to
      (let [buffer (.getChannelData buffers c)]
        ; for each individual sample in the count
        (doseq [s (range frame-count)]
          (let [t-seconds (/ s sample-rate)]
            ; mutate the audio buffer to add this sample value
            (aset buffer s
                  (sum-sample-buffers-at-time samples t-seconds))))
        (js/console.log buffer)
        ))
    buffers))

(defn make-channel-player [new-buffers]
  ; HTML5 audio buffer source
  (let [buffer-source (.createBufferSource actx)
        frame-count (.-length new-buffers)
        buffer-object (.createBuffer actx 2 frame-count sample-rate)]
    ; copy the data from one buffer to the other
    ; for left and right channels
    (doseq [c (range 2)]
      ; grab the raw js audio buffer for this channel to write to
      (let [buffer-dest (.getChannelData buffer-object c)
            buffer-source (.getChannelData new-buffers c)]
        ; for each individual sample in the count
        (doseq [s (range frame-count)]
          ; mutate the audio buffer to add this sample value
          (aset buffer-dest s (aget buffer-source s)))))
    ; set the buffer source on our new objects
    (set! (.-buffer buffer-source) buffer-object)
    ; set the loop flag on the buffer source
    (set! (.-loop buffer-source) true)
    ; connect it to the audio context output
    (.connect buffer-source (.-destination actx))
    ; pass back the thing that is playable
    buffer-source))

(defn make-channel
  "Create a new channel that can be used to play a loop of audio."
  []
  {:start-time nil :id (swap! channel-id inc) :buffers nil :player nil :loop-definition nil})

(defn update-channel-data
  "Update the audio data that a channel should play."
  [channel new-loop-definition]
  (let [{:keys [start-time id buffers player loop-definition] :as channel} channel
        ; create / update the audio buffers with the new data
        new-buffers (convert-loop-to-buffers channel loop-definition new-loop-definition)
        ; make a new player object
        new-player (make-channel-player new-buffers)
        ; update the channel definition with our changes
        modified-channel (assoc channel
                                :buffers new-buffers
                                :player new-player
                                :loop-definition new-loop-definition)]
    ; if the channel is already playing, continue playback at the correct sample
    (when start-time
      (.stop (channel :player))
      (.start (modified-channel :player (mod (- (get-audio-time) (channel :start-time)) (new-loop-definition :length-in-ms)))))
    modified-channel))

(defn play-channel
  "Start a channel playing - returns the modified channel."
  [channel]
  (let [now (get-audio-time)]
    (if (channel :player)
      (.start (channel :player))
      (print (str "Channel " (channel :id) " has no data yet!")))
    (assoc channel :start-time now)))
