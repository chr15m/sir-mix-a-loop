(ns ^:figwheel-always sir-mix-a-loop.dev
    (:require [sir-mix-a-loop.core :as mix-loop]))

(enable-console-print!)

(println "Sir Mix-a-loop dev mode.")

; (defonce app-state (atom {:text "Hello world!"}))

(defonce samples (atom []))

(def ticks 16)

(def my-loop {:length-in-ms 800})

(defonce looper
  (if mix-loop/audio?
    (let [looper (mix-loop/make-loop-player (:length-in-ms my-loop))]
      (.start (looper :buffer-source)) 
      looper)
    (js/alert "Audio not available in your browser, sorry!")))

(when looper
  (print "Mixing.")
  (mix-loop/update-loop looper my-loop))

(defn samples-watcher
  [watcher-key atom-changed old-state new-state]
  (mix-loop/update-loop looper (assoc my-loop :samples @samples)))

; when the samples atom is changed, update the loop
(defonce watcher
  (add-watch samples :loop-updater samples-watcher))

; create our user interface
(set! (.-innerHTML (js/document.getElementById "app"))
      (apply str (doall (for [e (range ticks)] (str "<span id='container-" e "'><input id='tick-" e "' type='checkbox'></input></span>")))))

; make a sample array of bonk sound
(defn make-bonk [p length]
  (let [sample-count (* mix-loop/sample-rate length)
        frequency-hz (mix-loop/mtof (+ 60 (mod (* p 5) 23)))]
    (doall (for [x (range sample-count)]
             ; envelope linear decay
             (* (/ (- sample-count x) sample-count)
                ; sine at the midi pitch specified
                (js/Math.sin (* (* (/ x mix-loop/sample-rate) frequency-hz) (* js/Math.PI 2))))))))

(defn make-tick-event-fn [e]
  (fn [ev]
    (reset! samples
            (doall (filter #(not (nil? %)) (for [e (range ticks)]
                                             (if (-> (js/document.getElementById (str "tick-" e)) .-checked)
                                               {:data (clj->js (make-bonk e 0.1))
                                                :start-seconds (* e 0.05)
                                                :length-seconds 0.1})))))))

; bind events
(doseq [e (range ticks)]
  (.addEventListener
    (js/document.getElementById (str "tick-" e))
    "click"
    (make-tick-event-fn e)
    false))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
