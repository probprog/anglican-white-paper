;; gorilla-repl.fileformat = 1

;; **
;;; Boiler-plate code --- importing necessary things.
;; **

;; @@
(use 'nstools.ns)
(ns+ deli
  (:like anglican-user.worksheet)
  (:require [anglican.stat :refer [mean std] ]))
;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[nil,nil]"}
;; <=

;; **
;;; ## The Deli Dilemma
;;; 
;;; A customer wearing round sunglasses came at 1:13pm, and grabbed a sandwitch and a coffee. Later on the same day, a customer wearing round sunglasses came at 6:09pm and ordered a dinner. Was it the same customer?
;;; 
;;; What we know:
;;; 
;;; * There is an adjacent office quarter, and it takes between 5 and 15 minutes from an office to the deli, varying for different buildings. 
;;; * Depending on traffic lights, the time varies by about 2 minutes.
;;; * The lunch break is at 1:00pm, and the workday ends at 6:00pm.
;;; * The waiter's odds that this is the same customer are 2 to 1.
;;; 
;;; Let's formalize this knowledge (times are in minutes):
;; **

;; @@
(def p-same "prior probability that this is the same customer" (/ 2. 3.))
(def mean-time-to-arrive "average time to arrive from the office quarter" 10)
(def sd-time-to-arrive "standard deviation of arrival time" 3.)
(def time-sd "time deviation" 1)
(def lunch-delay "time between lunch break and lunch order" 13)
(def dinner-delay "time between end of day and dinner order" 9)
;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;deli/p-same</span>","value":"#'deli/p-same"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/mean-time-to-arrive</span>","value":"#'deli/mean-time-to-arrive"}],"value":"[#'deli/p-same,#'deli/mean-time-to-arrive]"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/sd-time-to-arrive</span>","value":"#'deli/sd-time-to-arrive"}],"value":"[[#'deli/p-same,#'deli/mean-time-to-arrive],#'deli/sd-time-to-arrive]"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/time-sd</span>","value":"#'deli/time-sd"}],"value":"[[[#'deli/p-same,#'deli/mean-time-to-arrive],#'deli/sd-time-to-arrive],#'deli/time-sd]"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/lunch-delay</span>","value":"#'deli/lunch-delay"}],"value":"[[[[#'deli/p-same,#'deli/mean-time-to-arrive],#'deli/sd-time-to-arrive],#'deli/time-sd],#'deli/lunch-delay]"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/dinner-delay</span>","value":"#'deli/dinner-delay"}],"value":"[[[[[#'deli/p-same,#'deli/mean-time-to-arrive],#'deli/sd-time-to-arrive],#'deli/time-sd],#'deli/lunch-delay],#'deli/dinner-delay]"}
;; <=

;; @@
(defquery deli
  (let [time-to-arrive-prior (normal mean-time-to-arrive sd-time-to-arrive)
         same-customer (sample (flip p-same))]
    (predict :same-customer same-customer)
    (if same-customer
      ;; One customer
      (let [time-to-arrive (sample time-to-arrive-prior)]
        (observe (normal time-to-arrive time-sd) lunch-delay)
        (observe (normal time-to-arrive time-sd) dinner-delay)
        (predict :time-to-arrive time-to-arrive))
      ;; Two customers
      (let [time-to-arrive-1 (sample time-to-arrive-prior)
            time-to-arrive-2 (sample time-to-arrive-prior)]
        (observe (normal time-to-arrive-1 time-sd) lunch-delay)
        (observe (normal time-to-arrive-2 time-sd) dinner-delay)
        (predict :times-to-arrive [time-to-arrive-1 time-to-arrive-2])))))

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;deli/deli</span>","value":"#'deli/deli"}
;; <=

;; **
;;; Now, we lazily perform the inference.
;; **

;; @@
(def samples (doquery :pgibbs deli nil :number-of-particles 100))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;deli/samples</span>","value":"#'deli/samples"}
;; <=

;; **
;;; And retrieve predicts from the lazy sequence.
;; **

;; @@
(def N 10000)
(def predicts (map get-predicts (take N (drop N samples))))
;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;deli/N</span>","value":"#'deli/N"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/predicts</span>","value":"#'deli/predicts"}],"value":"[#'deli/N,#'deli/predicts]"}
;; <=

;; **
;;; Let's compute the probability that this is the same customer, and arrival times for each case:
;; **

;; @@
(def p-same+ (/ (count (filter :same-customer predicts)) (double N)))
                            

;; single customer                      
(def time-to-arrive+ (map :time-to-arrive (filter :same-customer predicts)))
(def mean-to-arrive+ (mean time-to-arrive+))
(def sd-to-arrive+ (std time-to-arrive+))

;; two customers
(def times-to-arrive+ (map :times-to-arrive 
                           (filter (complement :same-customer) predicts)))
(def mean-1-to-arrive+ (mean (map first times-to-arrive+)))
(def sd-1-to-arrive+ (std (map first times-to-arrive+)))
(def mean-2-to-arrive+ (mean (map second times-to-arrive+)))
(def sd-2-to-arrive+ (std (map second times-to-arrive+)))
;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;deli/p-same+</span>","value":"#'deli/p-same+"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/time-to-arrive+</span>","value":"#'deli/time-to-arrive+"}],"value":"[#'deli/p-same+,#'deli/time-to-arrive+]"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/mean-to-arrive+</span>","value":"#'deli/mean-to-arrive+"}],"value":"[[#'deli/p-same+,#'deli/time-to-arrive+],#'deli/mean-to-arrive+]"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/sd-to-arrive+</span>","value":"#'deli/sd-to-arrive+"}],"value":"[[[#'deli/p-same+,#'deli/time-to-arrive+],#'deli/mean-to-arrive+],#'deli/sd-to-arrive+]"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/times-to-arrive+</span>","value":"#'deli/times-to-arrive+"}],"value":"[[[[#'deli/p-same+,#'deli/time-to-arrive+],#'deli/mean-to-arrive+],#'deli/sd-to-arrive+],#'deli/times-to-arrive+]"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/mean-1-to-arrive+</span>","value":"#'deli/mean-1-to-arrive+"}],"value":"[[[[[#'deli/p-same+,#'deli/time-to-arrive+],#'deli/mean-to-arrive+],#'deli/sd-to-arrive+],#'deli/times-to-arrive+],#'deli/mean-1-to-arrive+]"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/sd-1-to-arrive+</span>","value":"#'deli/sd-1-to-arrive+"}],"value":"[[[[[[#'deli/p-same+,#'deli/time-to-arrive+],#'deli/mean-to-arrive+],#'deli/sd-to-arrive+],#'deli/times-to-arrive+],#'deli/mean-1-to-arrive+],#'deli/sd-1-to-arrive+]"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/mean-2-to-arrive+</span>","value":"#'deli/mean-2-to-arrive+"}],"value":"[[[[[[[#'deli/p-same+,#'deli/time-to-arrive+],#'deli/mean-to-arrive+],#'deli/sd-to-arrive+],#'deli/times-to-arrive+],#'deli/mean-1-to-arrive+],#'deli/sd-1-to-arrive+],#'deli/mean-2-to-arrive+]"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/sd-2-to-arrive+</span>","value":"#'deli/sd-2-to-arrive+"}],"value":"[[[[[[[[#'deli/p-same+,#'deli/time-to-arrive+],#'deli/mean-to-arrive+],#'deli/sd-to-arrive+],#'deli/times-to-arrive+],#'deli/mean-1-to-arrive+],#'deli/sd-1-to-arrive+],#'deli/mean-2-to-arrive+],#'deli/sd-2-to-arrive+]"}
;; <=

;; @@
(plot/histogram (map #(if (:same-customer %) 1 2) predicts)
                :bins 4 :x-title (format "number of o customers, p-same=%6g" p-same+))
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"24c88bcb-0827-422d-be8a-5253882145be","values":[{"x":1.0,"y":0},{"x":1.25,"y":1105.0},{"x":1.5,"y":0.0},{"x":1.75,"y":0.0},{"x":2.0,"y":0.0},{"x":2.25,"y":8895.0},{"x":2.5,"y":0}]}],"marks":[{"type":"line","from":{"data":"24c88bcb-0827-422d-be8a-5253882145be"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"24c88bcb-0827-422d-be8a-5253882145be","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"24c88bcb-0827-422d-be8a-5253882145be","field":"data.y"}}],"axes":[{"type":"x","scale":"x","title":"number of o customers, p-same=0.110500","titleOffset":30},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"24c88bcb-0827-422d-be8a-5253882145be\", :values ({:x 1.0, :y 0} {:x 1.25, :y 1105.0} {:x 1.5, :y 0.0} {:x 1.75, :y 0.0} {:x 2.0, :y 0.0} {:x 2.25, :y 8895.0} {:x 2.5, :y 0})}], :marks [{:type \"line\", :from {:data \"24c88bcb-0827-422d-be8a-5253882145be\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"24c88bcb-0827-422d-be8a-5253882145be\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"24c88bcb-0827-422d-be8a-5253882145be\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\", :title \"number of o customers, p-same=0.110500\", :titleOffset 30} {:type \"y\", :scale \"y\"}]}}"}
;; <=

;; **
;;; If there is a single customer, there is one arrival time, let's see how it is distributed:
;; **

;; @@
(plot/histogram  time-to-arrive+
                 :x-title (format "arrival time: mean=%6g sd=%6g" 
                                  mean-to-arrive+
                                  sd-to-arrive+))

;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"e5f5ede2-24aa-43af-8abc-019a1001c193","values":[{"x":9.00296853903584,"y":0},{"x":9.367719012472715,"y":24.0},{"x":9.732469485909592,"y":39.0},{"x":10.097219959346468,"y":52.0},{"x":10.461970432783344,"y":163.0},{"x":10.82672090622022,"y":198.0},{"x":11.191471379657097,"y":221.0},{"x":11.556221853093973,"y":202.0},{"x":11.92097232653085,"y":126.0},{"x":12.285722799967726,"y":54.0},{"x":12.650473273404602,"y":18.0},{"x":13.015223746841478,"y":6.0},{"x":13.379974220278354,"y":2.0},{"x":13.74472469371523,"y":0}]}],"marks":[{"type":"line","from":{"data":"e5f5ede2-24aa-43af-8abc-019a1001c193"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"e5f5ede2-24aa-43af-8abc-019a1001c193","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"e5f5ede2-24aa-43af-8abc-019a1001c193","field":"data.y"}}],"axes":[{"type":"x","scale":"x","title":"arrival time: mean=10.9355 sd=0.699844","titleOffset":30},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"e5f5ede2-24aa-43af-8abc-019a1001c193\", :values ({:x 9.00296853903584, :y 0} {:x 9.367719012472715, :y 24.0} {:x 9.732469485909592, :y 39.0} {:x 10.097219959346468, :y 52.0} {:x 10.461970432783344, :y 163.0} {:x 10.82672090622022, :y 198.0} {:x 11.191471379657097, :y 221.0} {:x 11.556221853093973, :y 202.0} {:x 11.92097232653085, :y 126.0} {:x 12.285722799967726, :y 54.0} {:x 12.650473273404602, :y 18.0} {:x 13.015223746841478, :y 6.0} {:x 13.379974220278354, :y 2.0} {:x 13.74472469371523, :y 0})}], :marks [{:type \"line\", :from {:data \"e5f5ede2-24aa-43af-8abc-019a1001c193\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"e5f5ede2-24aa-43af-8abc-019a1001c193\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"e5f5ede2-24aa-43af-8abc-019a1001c193\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\", :title \"arrival time: mean=10.9355 sd=0.699844\", :titleOffset 30} {:type \"y\", :scale \"y\"}]}}"}
;; <=

;; **
;;; For two customers there are two different time distributions, let's compare them.
;; **

;; @@
(plot/compose 
  (plot/histogram (map first times-to-arrive+) 
                  :x-title (format 
                             "arrival times: mean1=%6g, sd1=%6g; mean2=%6g, sd2=%6g" 
                             mean-1-to-arrive+ sd-1-to-arrive+
                             mean-2-to-arrive+ sd-2-to-arrive+)
                  :plot-range [[6 16] :all])
  (plot/histogram (map second times-to-arrive+)))
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":[6,16]},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"c8abd5f7-c01a-4499-86b1-a430e8db4387","field":"data.y"}}],"axes":[{"type":"x","scale":"x","title":"arrival times: mean1=12.7030, sd1=0.939033; mean2=9.17581, sd2=0.946528","titleOffset":30},{"type":"y","scale":"y"}],"data":[{"name":"c8abd5f7-c01a-4499-86b1-a430e8db4387","values":[{"x":6.0,"y":0},{"x":6.666666666666667,"y":0.0},{"x":7.333333333333334,"y":0.0},{"x":8.0,"y":0.0},{"x":8.666666666666666,"y":0.0},{"x":9.333333333333332,"y":0.0},{"x":9.999999999999998,"y":39.0},{"x":10.666666666666664,"y":121.0},{"x":11.33333333333333,"y":509.0},{"x":11.999999999999996,"y":1156.0},{"x":12.666666666666663,"y":2439.0},{"x":13.333333333333329,"y":2375.0},{"x":13.999999999999995,"y":1614.0},{"x":14.66666666666666,"y":455.0},{"x":15.333333333333327,"y":172.0},{"x":15.999999999999993,"y":15.0},{"x":16.66666666666666,"y":0.0},{"x":17.33333333333333,"y":0}]},{"name":"82a5c014-14a8-4799-a683-3e701bbc4b95","values":[{"x":5.619111215132279,"y":0},{"x":6.123837150707415,"y":5.0},{"x":6.6285630862825515,"y":35.0},{"x":7.133289021857688,"y":102.0},{"x":7.638014957432824,"y":355.0},{"x":8.142740893007959,"y":643.0},{"x":8.647466828583095,"y":1564.0},{"x":9.152192764158231,"y":1411.0},{"x":9.656918699733367,"y":2024.0},{"x":10.161644635308503,"y":1535.0},{"x":10.66637057088364,"y":717.0},{"x":11.171096506458776,"y":383.0},{"x":11.675822442033912,"y":90.0},{"x":12.180548377609048,"y":27.0},{"x":12.685274313184184,"y":3.0},{"x":13.19000024875932,"y":1.0},{"x":13.694726184334456,"y":0}]}],"marks":[{"type":"line","from":{"data":"c8abd5f7-c01a-4499-86b1-a430e8db4387"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"82a5c014-14a8-4799-a683-3e701bbc4b95"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain [6 16]} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"c8abd5f7-c01a-4499-86b1-a430e8db4387\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\", :title \"arrival times: mean1=12.7030, sd1=0.939033; mean2=9.17581, sd2=0.946528\", :titleOffset 30} {:type \"y\", :scale \"y\"}], :data ({:name \"c8abd5f7-c01a-4499-86b1-a430e8db4387\", :values ({:x 6.0, :y 0} {:x 6.666666666666667, :y 0.0} {:x 7.333333333333334, :y 0.0} {:x 8.0, :y 0.0} {:x 8.666666666666666, :y 0.0} {:x 9.333333333333332, :y 0.0} {:x 9.999999999999998, :y 39.0} {:x 10.666666666666664, :y 121.0} {:x 11.33333333333333, :y 509.0} {:x 11.999999999999996, :y 1156.0} {:x 12.666666666666663, :y 2439.0} {:x 13.333333333333329, :y 2375.0} {:x 13.999999999999995, :y 1614.0} {:x 14.66666666666666, :y 455.0} {:x 15.333333333333327, :y 172.0} {:x 15.999999999999993, :y 15.0} {:x 16.66666666666666, :y 0.0} {:x 17.33333333333333, :y 0})} {:name \"82a5c014-14a8-4799-a683-3e701bbc4b95\", :values ({:x 5.619111215132279, :y 0} {:x 6.123837150707415, :y 5.0} {:x 6.6285630862825515, :y 35.0} {:x 7.133289021857688, :y 102.0} {:x 7.638014957432824, :y 355.0} {:x 8.142740893007959, :y 643.0} {:x 8.647466828583095, :y 1564.0} {:x 9.152192764158231, :y 1411.0} {:x 9.656918699733367, :y 2024.0} {:x 10.161644635308503, :y 1535.0} {:x 10.66637057088364, :y 717.0} {:x 11.171096506458776, :y 383.0} {:x 11.675822442033912, :y 90.0} {:x 12.180548377609048, :y 27.0} {:x 12.685274313184184, :y 3.0} {:x 13.19000024875932, :y 1.0} {:x 13.694726184334456, :y 0})}), :marks ({:type \"line\", :from {:data \"c8abd5f7-c01a-4499-86b1-a430e8db4387\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"82a5c014-14a8-4799-a683-3e701bbc4b95\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}})}}"}
;; <=
