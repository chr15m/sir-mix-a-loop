(ns ^:figwheel-always sir-mix-a-loop.dev
    (:require [sir-mix-a-loop.core :as mix-loop]))

(enable-console-print!)

(println "Sir Mix-a-loop dev mode.")

; (defonce app-state (atom {:text "Hello world!"}))

(def my-loop {:length-in-ms 1000})

(defonce looper
  (if mix-loop/audio?
    (let [looper (mix-loop/make-loop-player (:length-in-ms my-loop))]
      (.start (looper :buffer-source)) 
      looper)
    (js/alert "Audio not available in your browser, sorry!")))

(when looper
  (print "Mixing.")
  (mix-loop/update-loop looper my-loop))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)

