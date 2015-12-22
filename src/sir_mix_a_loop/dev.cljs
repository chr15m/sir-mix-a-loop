(ns ^:figwheel-always sir-mix-a-loop.dev
    (:require [sir-mix-a-loop.core :as looper]
              [cljs.core.async :refer [<! timeout]])
    (:require-macros [cljs.core.async.macros :refer [go]]))

(enable-console-print!)

(println "Sir Mix-a-loop dev mode.")

(defonce player (atom nil))
(def ticks-length 16)

; *** LOOP REGENERATE *** ;

; taken from pure-data
(defn mtof [m]
  (cond
    (<= m -1500) 0
    (> m 1499) (mtof 1499)
    :else (* (js/Math.exp (* m .0577622650)) 8.17579891564)))

; make a sample array of bonk sound
(defn make-bonk [p length]
  (let [sample-count (* looper/sample-rate length)
        frequency-hz (mtof (+ 60 (mod (* p 5) 23)))
        result (vec (doall (for [x (range sample-count)]
                             (* 0.5
                                ; envelope linear decay
                                (/ (- (- sample-count 1) x) sample-count)
                                ; sine at the midi pitch specified
                                (js/Math.sin (* (* (/ x looper/sample-rate) frequency-hz) (* js/Math.PI 2)))))))]
    result))

; make a Float32Array of bonk sound
(defn make-bonk-float32 [p length]
  (let [sample-count (* looper/sample-rate length)
        frequency-hz (mtof (+ 60 (mod (* p 5) 23)))
        buffer (js/Float32Array. sample-count)
        result (loop [x 0]
                 (when (< x sample-count)
                   (aset buffer x
                         (* 0.5
                            ; envelope linear decay
                            (/ (- (- sample-count 1) x) sample-count)
                            ; sine at the midi pitch specified
                            (js/Math.sin (* (* (/ x looper/sample-rate) frequency-hz) (* js/Math.PI 2)))))
                   (recur (inc x))))]
    buffer))

(defn make-samples []
  (doall (filter
           ; don't add samples that are nil
           #(not (nil? %))
           ; create a sample for each tick in the range
           (for [t (range ticks-length)]
             ; if the checkbox is checked
             (if (-> (js/document.getElementById (str "tick-" t)) .-checked)
               ; create a new sample to go at this timeslot
               {:data (make-bonk-float32 t 1.0)
                :tick t})))))

(defn update-sample-data []
  (swap! player assoc-in [:pattern :samples] (make-samples)))

; *** UI **** ;

; create our user interface
(set! (.-innerHTML (js/document.getElementById "app"))
      (str
        (apply str (doall (for [e (range ticks-length)] (str "<span id='container-" e "'><input id='tick-" e "' type='checkbox'></input></span>"))))
        (str "<br/><button id='play-pause'>▶</button> <button id='stop'>◼</button>")))

; bind events to the page
(doseq [e (range ticks-length)]
  (.addEventListener
    (js/document.getElementById (str "tick-" e))
    "click"
    (fn [ev] (update-sample-data))
    false))

; play/pause button
(.addEventListener (js/document.getElementById "play-pause")
                   "click"
                   (fn [ev]
                     (if (@player :timing)
                       (do
                         (set! (.-innerHTML ev.target) "▶")
                         (looper/pause! player))
                       (do
                         (set! (.-innerHTML ev.target) "Ⅱ")
                         (looper/play! player))))
                   false)

; stop button
(.addEventListener (js/document.getElementById "stop")
                   "click"
                   (fn [ev]
                     (set! (.-innerHTML (js/document.getElementById "play-pause")) "▶")
                     (looper/stop! player))
                   false)

(defn set-class [n c]
  (set! (-> (js/document.getElementById n) .-className) c))

(defonce watcher
  (add-watch player
             :player-watcher
             (fn [k reference old-state new-state]
               (when (not (= (:tick old-state) (:tick new-state)))
                 (doseq [e (range ticks-length)]
                   (set-class (str "container-" e) ""))
                 (set-class (str "container-" (mod (:tick new-state) ticks-length)) "tick")))))

; *** Launch *** ;

(defn launch [new-loop-player]
  (reset! player new-loop-player)
  ; set up our new loop at 180 BPM and empty samples data
  (swap! player assoc-in [:pattern] {:bpm 180 :ticks-length ticks-length :samples []}))

; get a player (channel) to play a loop on
(if looper/audio?
  (launch (looper/make-loop-player))
  (js/alert "Audio not available in your browser, sorry!"))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
