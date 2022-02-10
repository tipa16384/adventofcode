(ns aoc.2016.1
  (:require [clojure.string :as str]))

(def compass [[0 1] [1 0] [0 -1] [-1 0]])

(defn new-heading [heading move]
  (mod (if (= (first move) \R) (inc heading) (dec heading)) 4))

(defn travel [heading arr]
  (if (empty? arr) []
      (into (vec (repeat (Integer/parseInt (subs (first arr) 1)) (new-heading heading (first arr))))
            (travel (new-heading heading (first arr)) (rest arr)))))

(defn journey [loc moves]
  (concat [loc]
          (if (empty? moves) []
              (journey [(+ (loc 0) ((first moves) 0)) (+ (loc 1) ((first moves) 1))] (rest moves)))))

(defn look_for_duplicates [path]
  (if (.contains (rest path) (first path)) (first path) (look_for_duplicates (rest path))))

(def head-seq (travel 0 (str/split (read-line) #",\s")))

(def path (journey [0 0] (map vector
                              (for [h head-seq] ((compass h) 0))
                              (for [h head-seq] ((compass h) 1)))))

(println "Part 1:" (+ (Math/abs ((last path) 0)) (Math/abs ((last path) 1))))

(def dupe (look_for_duplicates path))

(println "Part 2:" (+ (Math/abs (dupe 0)) (Math/abs (dupe 1))))
