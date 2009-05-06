;; Problem 1
;; Find the sum of all the multiples of 3 or 5 below 1000.

(define (euler1 max)
  (define (iter total current)
	(if (> current max) total
		(if (or (= (remainder current 3) 0)
				(= (remainder current 5) 0))
			(iter (+ total current)
				  (+ current 1))
			(iter total
				  (+ current 1)))))
  (iter 0 1))