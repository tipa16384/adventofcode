(ns aoc.2015.1
  (:require [clojure.string :as str]))

(def compass [[0 1] [1 0] [0 -1] [-1 0]])
(defn go-left [heading] (if (= heading 0) (dec (count compass)) (dec heading)))
(defn go-right [heading] (mod (inc heading) (count compass)))
(def arr (str/split (str/replace (read-line) #"," "") #"\s"))



(defn take-a-walk [directions]
  (def x 0)
  (def y 0)
  (def heading 0)
  (doseq [move directions]
    (def turn (first move))
    (def heading (if (= turn \L) (go-left heading) (go-right heading)))
    (def dist (Integer/parseInt (subs move 1)))
    (def x (+ x (* dist ((compass heading) 0))))
    (def y (+ y (* dist ((compass heading) 1)))))
  (+ (Math/abs x) (Math/abs y)))

(defn take-another-walk [directions]
  (def move-record (set []))
  (def x 0)
  (def y 0)
  (def heading 0)
  (doseq [move directions]
    (when (not (contains? move-record [x y]))
    (def turn (first move))
     (def heading (if (= turn \L) (go-left heading) (go-right heading)))
     (def dist (Integer/parseInt (subs move 1)))
     (def move-record (conj move-record [x y]))
     (def x (+ x (* dist ((compass heading) 0))))
     (def y (+ y (* dist ((compass heading) 1))))
            (println move x y)
))
  (println "We have already visited" x y)
  (+ (Math/abs x) (Math/abs y)))

(println "Part 1:" (take-a-walk arr))
(println "Part 2:" (take-another-walk arr))
