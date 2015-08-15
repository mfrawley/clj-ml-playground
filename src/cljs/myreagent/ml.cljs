(ns myreagent.ml)

(def theta0 0.5)
(def theta1 5.9)

; Training set is simple straight line parallel to x-axis
(def training-data [[10.0 20.0], [20.0 40.0], [40.0 80.0], [80.0 160.0]])

(defn square [x]
  (* x x))

(defn sum [coll]
  (reduce + coll))

(defn hypothesis-func [x theta0 theta1]
  (+ theta0 (* theta1 x)))
; (defn hypothesis-func [x theta0 theta1]
;   (* theta1 x))

(defn sq-error [tuple theta0 theta1]
  (let [[x y] tuple]
  (square (- (hypothesis-func x theta0 theta1) y))))

(defn hypothesis-line [data]
    (mapv
      (fn [tuple]
        (let [[x y] tuple]
          [x (hypothesis-func x theta0 theta1)]))
     data))

(defn cost-fn [data theta0 theta1]
  (let [m (count data)]
    (try
      (/ 1 (* (* 2 m)
        (sum
          (mapv
            (fn [tuple]
              (sq-error tuple theta0 theta1))
            data))))
            (catch js/Error e
              ; (println e)
              0))))
