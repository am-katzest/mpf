(ns core
  (:require [opencv4.core :as cv]
            [clojure.java.shell :as sh])
  (:import [org.opencv.core Mat]))

(def order-finished-icon (cv/imread "foxholefinished"))

(def order-running-icon (cv/imread "foxholenotfinished"))

(defn matches? [image template]
  (let [out (Mat/zeros (cv/size image) 0)]
    (cv/match-template
     image
     template
     out
     cv/TM_SQDIFF)
    (< (.minVal (cv/min-max-loc out)) 100000)))

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
    (cv/imread filename)))

(defn analyze-screenshot [image]
  (case [(has-completed-order? image) (has-running-order? image)]
    [true true] :mixed
    [true false] :completed
    [false true] :running
    [false false] :none))

(def queue-status (comp analyze-screenshot take-screenshot))

(defn notify [s]
  (println "notifying:" s)
  (sh/sh "notify-send" s))

(defn nofoxhole [window-id]
  (if window-id :inactive :active))

(defn inactive [window-id]
  (let [queue (queue-status window-id)]
    (cond
      (nil? window-id) :nofoxhole
      (= :none queue) :inactive
      :else (do (notify "yayy, queues!") :active))))

(defn active [window-id]
  (let [queue (queue-status window-id)]
    (cond
      (nil? window-id) :nofoxhole
      (= :none queue) (do (notify "queues no longer visible") :inactive)
      (= :completed queue) (do (notify "finished!") :finished)
      :else :active)))

(defn finished [window-id]
  (let [queue (queue-status window-id)]
    (cond
      (nil? window-id) :nofoxhole
      (= :completed queue) :finished
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

(defn run-loop []
  (loop [state :inactive]
    (println "state:" state)
    (Thread/sleep (state->wait-time state))
    (recur ((state->fn state) (foxhole-window-id)))))

(run-loop)
