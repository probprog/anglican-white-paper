
(defn  fib [n]
  (loop [a 0 b 1 m 0]
        (if (= m n)
          a
          (recur b (+ a b) (inc m)))))

(with-primitive-procedures [fib]
  (defquery  branching  []
    (let [count-prior (poisson  4)
          r (sample  count-prior)
          l (if (< 4 r)
              6
              (+ (fib (* 3 r))
                 (sample  count-prior )))]
      (observe (poisson l) 6)
      (predict :r r))))
