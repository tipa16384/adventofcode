(def data (slurp "puzzle1.dat"))
(println "There are " (count data) " characters in the file.")
(println "The first character is " (first data))
(println "The first ten characters are " (take 10 data))
(def freq (frequencies data))
(println "Part 1:" (- (freq \() (freq \))))
(comment "map ( to 1 and ) to -1")
(def freq2 (map (fn [x] (if (eq x ?\() 1 -1) x)) data))
(println "First ten mapped are " (take 10 freq2))
