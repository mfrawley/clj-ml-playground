(def true-positives 85)
(def false-positives 890)
(def false-negatives 15)

(defn precision [true-pos false-pos] (/ true-pos (+ true-pos false-pos)))
(defn recall [true-pos false-neg] (/ true-pos (+ true-pos false-neg)))

(def this-precision (precision true-positives false-positives))
(def this-recall (recall true-positives false-negatives))
(println this-recall)
(println this-precision)

(def f-score (/ (* 2 this-precision this-recall) (+ this-precision this-recall))) 
(println f-score)
