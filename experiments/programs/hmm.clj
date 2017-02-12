(use 'nstools.ns)
(ns+ hmm
  (:like anglican-user.program))

(def observations 
    [0.9 0.8 0.7 0.0
     -0.025 -5.0 -2.0 -0.1
     0.0 0.13 0.45 6
     0.2 0.3 -1 -1])

(def init-dist (discrete [1.0 1.0 1.0]))

(def trans-dists [(discrete [0.1 0.5 0.4])
                  (discrete [0.2 0.2 0.6])
                  (discrete [0.15 0.15 0.7])])

(def obs-dists [(normal -1 1)
                (normal 1 1)
                (normal 0 1)])

(defquery hmm 
  (predict
    :states
    (reduce
      (fn [states obs]
          (let [state (sample (get trans-dists
                                   (peek states)))]
            (observe (get obs-dists state) obs)
            (conj states state)))
      [(sample init-dist)]
      observations)))
