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

(use '(incanter core charts pdf stats))

(def zero '(0))
(def sp (xy-plot zero zero :title "Graph" :x-label "X" :y-label "Y" :series-label "" :legend true :points true ))
(add-lines sp '(1 2 3) '(40 50 60) :series-label "Big")
(add-lines sp '(1 2 3) (exponential-smooth '(40 50 60) 0.9) :series-label "Big Smooth")
(add-lines sp '(1 2 3 17) '(20 55 60 18) :series-label "Small")
(add-lines sp (range 1 100) (range 1 100) :series-label "500n")
(add-lines sp (range 1 100) (exponential-smooth (range 1 100) 0.9) :series-label "500s")
(view sp)
(save-pdf (function-plot sin -4 4) "/tmp/incanter-sample.pdf")
(save-pdf sp "/tmp/incanter-sample2.pdf")
