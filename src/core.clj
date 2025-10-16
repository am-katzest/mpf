(ns core
  (:require [opencv4.core :as cv]
            [clojure.java.shell :as sh])
  (:import [org.opencv.core Mat]))

(defn edges [image]
  (-> image
      (cv/sobel! cv/CV_8U, 1, 1)
      (cv/cvt-color! cv/COLOR_BGR2GRAY)
      (cv/threshold! 6 255 cv/THRESH_BINARY)))


(defn load-image [filename]
  (edges (cv/imread filename)))

(def order-finished-icon (load-image "foxholefinished"))

(def order-running-icon (load-image "foxholenotfinished"))

(defn matches? [image template]
  (let [out (Mat/zeros (cv/size image) 0)]
    (cv/match-template
     image
     template
     out
     cv/TM_SQDIFF)
    (< (.minVal (cv/min-max-loc out)) 500000)))

(defn has-completed-order? [image]
  (matches? image order-finished-icon))

(defn has-running-order? [image]
  (matches? image order-running-icon))

(defn foxhole-window-id []
  (let [{:keys [exit out]} (sh/sh "xdotool" "search"  "--name" "War  ")]
    (when (= 0 exit) (.trim out))))

(defn take-screenshot [window-id]
  (let [filename "/tmp/mpf.png"]
    (sh/sh "import" "-silent" "-window" window-id "-crop" "100x550+1520+150" filename)
    (load-image filename)))

(defn analyze-screenshot [image]
  (case [(has-completed-order? image) (has-running-order? image)]
    [true true] :mixed
    [true false] :completed
    [false true] :running
    [false false] :none))

(def queue-status (comp analyze-screenshot take-screenshot))


(defn beep []
  (sh/sh "mpg123" "-f5000" "notify.mp3"))

(defn nofoxhole [window-id]
  (if window-id :inactive :nofoxhole))

(defn inactive [window-id]
  (let [queue (queue-status window-id)]
    (cond
      (nil? window-id) :nofoxhole
      (= :none queue) :inactive
      :else (do (println "yayy, queues!") :active))))

(defn active [window-id]
  (let [queue (queue-status window-id)]
    (cond
      (nil? window-id) :nofoxhole
      (= :none queue) (do (println "queues no longer visible") :inactive)
      (= :completed queue) (do (beep) (println "finished!") :finished)
      :else :active)))

(defn finished [window-id]
  (let [queue (queue-status window-id)]
    (cond
      (nil? window-id) :nofoxhole
      (= :completed queue) (do (beep) :finished)
      :else :inactive)))

(def state->wait-time
  (let [s 1000]
   {:active (* 2 s)
    :finished (* 5 s)
    :nofoxhole (* 2 60 s)
    :inactive (* 20 s)}))

(def state->fn
  {:active active
   :nofoxhole nofoxhole
   :finished finished
   :inactive inactive})

(defn run-loop [& _args]
  (loop [state :inactive]
    (println "state:" state)
    (Thread/sleep (state->wait-time state))
    (recur ((state->fn state) (foxhole-window-id)))))
