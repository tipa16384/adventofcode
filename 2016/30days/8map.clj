(def source (for [_ (range (* 2 (read)))] (read)))
(def book (zipmap (take-nth 2 source) (take-nth 2 (rest source))))
(def done false)
(while (not done)
  (def ln (read *in* false nil))
  (if ln (println
          (if (contains? book ln) (format "%s=%d" ln (book ln)) "Not found"))
      (def done true)))
