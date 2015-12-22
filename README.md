WebAudio looping sequencer in ClojureScript.

	; atom to store our loop player
	(defonce player (atom nil))
	
	; define some pattern of samples to be played
	; e.g. two different sounds at tick 0 and tick 3 respectively
	; where the :data key specifies a sample as an array of floats
	; :data can also be a Float32Array which is mixed more efficiently
	(def my-samples [{:tick 0 :data (make-sin-wave 440 0.1)}
	                 {:tick 3 :data (make-sin-wave 880 0.2)}])
	
	; first check if the user's browser has webaudio capability
	(when looper/audio?
	    ; make a new loop player and put it in our atom
	    (reset! player
		    (looper/make-loop-player))
	    ; set up our new loop pattern at 180 BPM, looping every 16 ticks
	    ; use the sequence of samples we defined above
	    (swap! player assoc-in [:pattern] {:bpm 180
	                                       :ticks-length 16
	                                       :samples my-samples})
	    ; start our player
	    (looper/play! player))

