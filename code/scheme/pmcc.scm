(define (sxy sumx sumy sumxy n)
  (- sumxy (/ (* sumx sumy) n)))

(define (pmcc sumx sumy sumxy sumxsq sumysq n)
  (/ (sumxy sumx sumy sumxy n)
	 (sqrt (*
			(sumxy sumx sumx sumxsq n)
			(sumxy sumy sumy sumysq n)))))