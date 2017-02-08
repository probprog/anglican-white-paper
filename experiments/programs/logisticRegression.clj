(use 'nstools.ns)
(ns+ logisticRegression
  (:like anglican-user.program))

(def xs [-10 -5 2 6 10])
(def labels [false false true true true])

(defquery logisticRegression 
  (let [m (sample (normal 0 1))
        b (sample (normal 0 1))
        sigma (sample (gamma 1 1))
        y (fn [x] (sample (normal (+ (* m x) b) sigma)))
        sigmoid (fn [x] (/ 1. (+ 1. (exp (* -1. (y x))))))]
   (loop [data (map #(-> [%1 %2]) xs labels)]
    (when (seq data)
     (let [[[x label] & data] data] 
       (observe (flip (sigmoid x)) label))))
   (predict (sigmoid 8))))
