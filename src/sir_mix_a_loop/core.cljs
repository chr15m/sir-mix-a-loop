(ns sir-mix-a-loop.core)

(defn calculate-frame-count [length-in-ms sample-rate]
  (* (* length-in-ms 0.001) sample-rate))

; TODO: only define this once
; check for AudioContext availability and set
(if-let [actx (when (aget js/window "AudioContext") (js/window.AudioContext.))]
  ; variables everything will use
  (let [sample-rate (.-sampleRate actx)]
    ; flag that audio is available
    (def audio? true)
    
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
            frame-count (calculate-frame-count length-in-ms sample-rate)
            pitch (* (js/Math.random) 0.1)]
        (print pitch)
        ; for left and right
        (doseq [c (range 2)]
          (let [buffer (.getChannelData buffer-object c)]
            (doseq [s (range frame-count)]
              ; mutate the audio buffer!
              (aset buffer s (Math.sin (* s pitch))))))
        ; force browsers to refresh the buffer
        (try
          (set! (.-buffer buffer-source) buffer-object)
          ; catch this: Uncaught TypeError: Failed to set the 'buffer' property on 'AudioBufferSourceNode'
          (catch :default e e)))))
  
  ; flag that audio is not available
  (def audio? false))
