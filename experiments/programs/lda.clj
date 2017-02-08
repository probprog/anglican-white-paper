(use 'nstools.ns)
(ns+ lda
  (:like anglican-user.program)
  (:require [clojure.string :refer [split]]))

(def vocabulary ["bear" "wolf" "python" "prolog"])
(def topics ["topic1" "topic2"])

(def docs
  [(split "bear wolf bear wolf bear wolf python wolf bear wolf" #" ")
   (split "python prolog python prolog python prolog python prolog python prolog" #" ")
   (split "bear wolf bear wolf bear wolf bear wolf bear wolf" #" ")
   (split "python prolog python prolog python prolog python prolog python prolog" #" ")
   (split "bear wolf bear python bear wolf bear wolf bear wolf" #" ")])

(defquery lda
  "lda, following WebPPL example"
  (let [x (sample (bernoulli 0.5))]
    (observe (normal x 1.) 0.8)
    (predict x)))

(def word-dist-prior (dirichlet (repeat (count vocabulary) 1.0)))
(def topic-dist-prior (dirichlet (repeat (count topics) 1.0)))

(def word-index (into {} (map #(-> [%1 %2]) vocabulary (range))))

(defquery lda 
  (let [word-dist-for-topic
        (into {} (map #(-> [% (discrete 
                                (sample word-dist-prior))])
                      (range (count topics))))]

    (loop [docs docs]
      (when (seq docs)
        (let [[doc & docs] docs
              topic-dist (discrete (sample topic-dist-prior))]
          (loop [words doc]
            (when (seq words)
              (let [[word & words] words
                    topic (sample topic-dist)]
                (observe (get word-dist-for-topic topic)
                         (get word-index word))
                (recur words))))
          (recur docs))))

    (predict word-dist-for-topic)))
