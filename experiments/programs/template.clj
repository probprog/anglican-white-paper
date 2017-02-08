(use 'nstools.ns)
(ns+ template
  (:like anglican-user.program))

(defquery template
  "query template"
  (let [x (sample (bernoulli 0.5))]
    (observe (normal x 1.) 0.8)
    (predict x)))
