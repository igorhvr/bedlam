; From https://gist.github.com/pingles/1160386
(defn exponential-smooth
	"
Smooths a sequence with an exponentially decreasing weight.
 
Arguments:
x -- a sequence of numbers
f -- the smoothing factor (between 0 and 1). close to 0 will produce
a stronger smoothing effect less responsive to recent values. closer
to 1 will have less of an effect and give greater weight to recent
values.
 
Examples:
(exponential-smooth (range 1 500) 0.9)
 
References:
http://en.wikipedia.org/wiki/Smoothing
http://en.wikipedia.org/wiki/Exponential_smoothing
"
[x f]
(letfn [
	(smooth
	  [alpha st xt]
	  (conj st
		(+
		 (* alpha xt)
		 (* (- 1 alpha) (last st)))))]
  (reduce (partial smooth f) [(first x)] (rest x))))

(use '(incanter core charts pdf stats io datasets))

(def zero '(0))
(def sp (xy-plot zero zero :title "Graph" :x-label "X" :y-label "Y" :series-label "Origin" :legend true :points true ))
(add-lines sp '(1 2 3) '(40 50 60) :series-label "Big")
(add-lines sp '(1 2 3) (exponential-smooth '(40 50 60) 0.9) :series-label "Big Smooth")
(add-lines sp '(1 2 3 17) '(20 55 60 18) :series-label "Small")
(add-lines sp (range 1 100) (range 1 100) :series-label "500n")
(add-lines sp (range 1 100) (exponential-smooth (range 1 100) 0.9) :series-label "500s")
(view sp)
(save-pdf (function-plot sin -4 4) "/tmp/incanter-sample.pdf")
(save-pdf sp "/tmp/incanter-sample2.pdf")

(def sp (xy-plot zero zero :title "Graph" :x-label "X" :y-label "Y" :series-label "Origin" :legend true :points true ))
(def x (range 1 124))
(def y '(110037.79 129339.24 107991.13 104663.33 121791.56 117404.57 99787.64 113333.58 105582.06 107038.34 106370.12 104274.88 76651.85 84340.84 94929.81 98283.50 94908.58 66321.24 86807.29 75568.31 100712.94 79386.93 93991.23 80598.22 77522.40 86441.63 68778.69 80069.78 75080.85 84561.45 84412.78 93952.75 101442.08 54485.30 83813.13 89306.17 110721.20 98308.10 91392.99 96493.24 82183.24 99937.03 79298.10 72851.50 72236.61 63931.69 89302.24 71595.11 71862.39 72791.01 68161.02 89189.86 56194.63 71004.84 62473.88 101096.61 78973.60 66804.82 70889.38 55372.44 64624.99 71957.51 104350.06 81390.16 77695.34 78743.36 72456.08 71477.51 72260.51 76007.18 72581.23 89345.90 70895.08 59929.43 62636.18 40228.01 70767.96 59389.82 64411.25 61430.89 50840.94 67449.67 43251.91 58080.28 58162.37 86748.88 58410.45 47789.01 44909.72 49865.12 52339.84 58013.43 72051.93 80497.70 53975.58 57271.57 53343.52 79258.35 71528.99 70323.53 65039.49 46880.28 62859.62 41578.78 44388.41 49093.82 47935.69 63191.25 42498.44 50636.03 46453.21 57764.63 48602.45 52348.20 62052.34 46252.56 76931.32 44980.00 52199.81 56754.63 57080.13 57507.24 31475.39 ))
(add-lines sp x y  :series-label "br")
;;(def lm (linear-model x y))(add-lines sp x (:fitted lm) :series-label "brlm")
(view sp)

(use '(clj-pdf [core :as clj-pdf-core]
               [svg :as clj-pdf-svg]
               [charting :as clj-pdf-charting]
               [graphics-2d :as clj-pdf-graphics-2d]))
               
;;;       core  as svg charting graphics-2d))

(defn test-plotx
 []
 (let [x '(1 2 3 4)
       y '(1 3 5 7)
       plot1 (scatter-plot x y)
       out (new java.io.ByteArrayOutputStream)]
    (org.jfree.chart.ChartUtilities/writeScaledChartAsPNG out plot1 300 200 1 1)    
    (.toByteArray out)))

(defn chart-to-byte-array-png-image 
  [crt width height]
  (let [out (new java.io.ByteArrayOutputStream)]
    (org.jfree.chart.ChartUtilities/writeScaledChartAsPNG out crt width height 1 1)
    (.toByteArray out)))


(pdf [{:title "Data" :size :b0 } [:table 
          [[:image (test-plotx)]                               [:image (chart-to-byte-array-png-image sp 500 500) b]]
          [[:image (test-plotx)]                               [:image (chart-to-byte-array-png-image sp 350 350) ]]
          [[:image (chart-to-byte-array-png-image sp 350 350) ][:image (test-plotx)]]
          ]]
     "/tmp/test7.pdf")
