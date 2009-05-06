;; Exercise 1.3.  Define a procedure that takes three numbers as arguments and returns the sum of the squares of the two larger numbers.

(define (bigsquare a b c)
  (cond ((and (> a c) (> b c)) (+ (* a a) (* b b)))
		((and (> a b) (> c b)) (+ (* a a) (* c c)))
		(else (+ (* b b) (* c c)))))