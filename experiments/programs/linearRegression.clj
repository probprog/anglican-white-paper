(use 'nstools.ns)
(ns+ linearRegression
  (:like anglican-user.program))

(def xs [0 1 2 3])
(def ys [0 1 4 6])

(defquery linearRegression 
  (let [m (sample (normal 0 2))
        b (sample (normal 0 2))
        sigma (sample (gamma 1 1))
		f (fn [x] (+ (* m x) b))]
   (loop [data (map #(-> [%1 %2]) xs ys)]
    (when (seq data)
     (let [[[x y] & data] data] 
       (observe (normal (f x) sigma) y)
       (recur data)))
   (predict (f 4))))
