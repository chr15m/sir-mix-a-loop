(ns ^:figwheel-always sir-mix-a-loop.dev
    (:require [sir-mix-a-loop.core :as mix-loop]))

(enable-console-print!)

(println "Sir Mix-a-loop dev mode.")

(defonce samples (atom []))
(def ticks 16)
(def my-loop {:length-in-ms 800})

; get our channel to play a loop on
(defonce channel
  (atom
    (if mix-loop/audio?
      (mix-loop/make-channel)
      (js/alert "Audio not available in your browser, sorry!"))))

; if we have audio available
; mix the first loop
(when @channel
  (print "Mixing.")
  (swap! channel mix-loop/update-channel-data my-loop)
  (swap! channel mix-loop/play-channel))

; *** LOOP REGENERATE *** ;

; make a sample array of bonk sound
(defn make-bonk [p length]
  (let [sample-count (* mix-loop/sample-rate length)
        frequency-hz (mix-loop/mtof (+ 60 (mod (* p 5) 23)))
        result (doall (for [x (range sample-count)]
                        ; envelope linear decay
                        (* 0.5 (/ (- (- sample-count 1) x) sample-count)
                           ; sine at the midi pitch specified
                           (js/Math.sin (* (* (/ x mix-loop/sample-rate) frequency-hz) (* js/Math.PI 2))))))]
    (print (last result))
    (print sample-count)
    result))

; function that gets run when the loop is changed
(defn samples-watcher
  [watcher-key atom-changed old-state new-state]
  (swap! channel mix-loop/update-channel-data
         (assoc my-loop :samples @samples)))

; when the samples atom is changed, update the loop
(defonce watcher
  (add-watch samples :loop-updater samples-watcher))

; *** UI **** ;

; create our user interface
(set! (.-innerHTML (js/document.getElementById "app"))
      (apply str (doall (for [e (range ticks)] (str "<span id='container-" e "'><input id='tick-" e "' type='checkbox'></input></span>")))))

; what happens when one of the checkboxes is clicked
(defn make-checkbox-event-fn [e]
  (fn [ev]
    (reset! samples
            (doall (filter #(not (nil? %)) (for [e (range ticks)]
                                             (if (-> (js/document.getElementById (str "tick-" e)) .-checked)
                                               {:data (clj->js (make-bonk e 0.5))
                                                :start-seconds (* e 0.05)
                                                :length-seconds 0.5})))))))

; bind events to the page
(doseq [e (range ticks)]
  (.addEventListener
    (js/document.getElementById (str "tick-" e))
    "click"
    (make-checkbox-event-fn e)
    false))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
