(use 'nstools.ns)
(ns+ multivariateRegression
  (:like anglican-user.program)
  (:require [clojure.core.matrix :as m]))

(def xs (map m/array [[0. 1.] [1. 1.5] [2. 2.] [3. 2.5] [4. 3.]]))
(def ys [0.5 2.8, 5., 7.3, 9.5])

(defquery multivariateRegression 
  (declare :ns-primitive m)
  (let [cov (m/identity-matrix 2)
        m (sample (mvn (m/zero-vector 2) cov))
        e (sample (normal 0 2))
        sigma (sample (gamma 1 1))
		f (fn [x] (+ (m/esum (m/mmul m x)) e))]
   (loop [data (map #(-> [%1 %2]) xs ys)]
    (when (seq data)
     (let [[[x y] & data] data] 
       (observe (normal (f x) sigma) y)
       (recur data)))
   (predict (f (m/array [5 4])))))
