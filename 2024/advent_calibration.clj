(ns advent_calibration
  (:require [clojure.string :as str]))

;; Recursive function to determine if the target can be achieved
(defn evaluate-recursive [numbers target current]
  (if (empty? numbers)
    (= current target)
    (let [next-number (first numbers)
          remaining (rest numbers)]
      (or
        ;; Try addition
        (evaluate-recursive remaining target (+ current next-number))
        ;; Try multiplication
        (evaluate-recursive remaining target (* current next-number))
        ;; Try concatenation
        (when (not= current 0)
          (evaluate-recursive remaining target
                              (Long/parseLong (str current next-number))))))))

;; Process a single line to determine if it matches the target
(defn process-line [line]
  (let [[target-str numbers-str] (str/split line #": ")
        target (Long/parseLong target-str)
        numbers (mapv #(Integer/parseInt %) (str/split numbers-str #" "))]
    (if (evaluate-recursive numbers target 0)
      target
      0)))

;; Calculate the total calibration result
(defn calculate-total-calibration [lines]
  (reduce + (map process-line lines)))

;; Main function
(defn -main [& args]
  (let [lines (->> "in.txt"
                   slurp
                   str/split-lines)
        start-time (System/nanoTime)
        total-calibration (calculate-total-calibration lines)
        end-time (System/nanoTime)]
    (println "Total Calibration Result:" total-calibration)
    (printf "Execution Time: %.6f seconds\n"
            (/ (- end-time start-time) 1e9))))
