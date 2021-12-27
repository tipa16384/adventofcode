(comment
    ;; return the count of the elements of an array where the current element is
    ;; greater than the previous element
    (defn count-greater-than-previous [array]
      (if (empty? array)
          0
          (let [prev (first array)]
            (reduce (fn [count element]
                      (if (> element prev)
                          (+ count 1)
                          count))
                    0
                    (rest array)))))
    ;; read an array of integers from puzzle1a.dat, one integer per line
    (defn read-puzzle1a-data [file]
      (let [lines (read-lines file)]
        (map (fn [line]
               (read line))
             lines)))
)

