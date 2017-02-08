;; gorilla-repl.fileformat = 1

;; **
;;; Boiler-plate code --- importing necessary things.
;; **

;; @@
(use 'nstools.ns)
(ns+ towers-of-hanoi
  (:like anglican-user.worksheet))
;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[nil,nil]"}
;; <=

;; **
;;; Towers of Hanoi in Clojure.
;; **

;; @@
(defn towers-of-hanoi-c [n from to via]
  (when (not= n 1)
      (towers-of-hanoi-c (dec n) from via to)
      (towers-of-hanoi-c (dec n) via to from)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;towers-of-hanoi/towers-of-hanoi-c</span>","value":"#'towers-of-hanoi/towers-of-hanoi-c"}
;; <=

;; **
;;; Towers of Hanoi in Anglican
;; **

;; @@
(defm towers-of-hanoi-a [n from to via]
  (when (not= n 1)
      (towers-of-hanoi-a (dec n) from via to)
      (towers-of-hanoi-a (dec n) via to from)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;towers-of-hanoi/towers-of-hanoi-a</span>","value":"#'towers-of-hanoi/towers-of-hanoi-a"}
;; <=

;; **
;;; Number of times to repeat the experiment.
;; **

;; @@
(def NREPEAT 1)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;towers-of-hanoi/NREPEAT</span>","value":"#'towers-of-hanoi/NREPEAT"}
;; <=

;; **
;;; Measure times for Clojure.
;; **

;; @@
(doseq [n [10 15 20 25]]
  (time (doall (repeatedly NREPEAT #(towers-of-hanoi-c n 0 1 2)))))
;; @@
;; ->
;;; &quot;Elapsed time: 0.612861 msecs&quot;
;;; &quot;Elapsed time: 3.612076 msecs&quot;
;;; &quot;Elapsed time: 60.852401 msecs&quot;
;;; &quot;Elapsed time: 1752.210869 msecs&quot;
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; Measure times for Anglican.
;; **

;; @@
(doseq [n [10 15 20 25]]
  (time (doall (take NREPEAT (doquery :importance (query (towers-of-hanoi-a n 0 1 2)) nil)))))
;; @@
;; ->
;;; &quot;Elapsed time: 4.299429 msecs&quot;
;;; &quot;Elapsed time: 5.874907 msecs&quot;
;;; &quot;Elapsed time: 110.541719 msecs&quot;
;;; &quot;Elapsed time: 3332.169503 msecs&quot;
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=
