(ns sir-mix-a-loop.core)

; check for AudioContext availability and set
(defonce actx (when (aget js/window "AudioContext") (js/window.AudioContext.)))

; flag for the user to test if audio is available
(defonce audio? (if actx true false))

(defonce sample-rate (if actx (.-sampleRate actx) nil))

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

(defn sum-sample-buffers-at-time [samples t-seconds]
  (doseq [{:keys [data start-seconds length-seconds volume pan sample-sample-rate] :or {volume 1.0 pan 0.0 sample-sample-rate sample-rate}} samples]
    (if (and (> t-seconds start-seconds) (< t-seconds (+ start-seconds length-seconds)))
      (* (aget data (* (- t-seconds start-seconds) sample-sample-rate)) volume))))

(defn make-loop-player [length-in-ms]
  ; HTML5 audio buffer source
  (let [buffer-source (.createBufferSource actx)
        frame-count (calculate-frame-count length-in-ms sample-rate)
        buffer-object (.createBuffer actx 2 frame-count sample-rate)]
    ; set the loop flag on the buffer source
    (set! (.-loop buffer-source) true)
    ; connect it to the audio context output
    (.connect buffer-source (.-destination actx))
    {:buffer-source buffer-source
     :buffer-object buffer-object}))

(defn update-loop [loop-player loop-definition]
  (let [{:keys [length-in-ms samples]} loop-definition
        {:keys [buffer-source buffer-object]} loop-player
        frame-count (calculate-frame-count length-in-ms sample-rate)]
    ; for left and right channels
    (doseq [c (range 2)]
      ; grab the raw js audio buffer for this channel to write to
      (let [buffer (.getChannelData buffer-object c)]
        ; for each individual sample in the count
        (doseq [s (range frame-count)]
          (let [t-seconds (/ s sample-rate)]
            ; mutate the audio buffer to add this sample value
            (aset buffer s
                  (sum-sample-buffers-at-time samples t-seconds))))))
    ; force browsers to refresh the buffer
    (try
      (set! (.-buffer buffer-source) buffer-object)
      ; catch this on Chrome: "Uncaught TypeError: Failed to set the 'buffer' property on 'AudioBufferSourceNode'" w@ it works anyway
      (catch :default e e))))
