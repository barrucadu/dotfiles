;; Problem 2
;; Find the sum of all the even-valued terms in the Fibonacci sequence which do not exceed four million.

(load "/home/barrucadu/code/scheme/generic.scm")

(define (euler2 max)
  (define (iter total a b)
	(cond ((>= a max) total)
		  ((and (even? a) (< a max))
		   (iter (+ total a) b
				  (+ a b)))
		   (else (iter total b
				  (+ a b)))))
  (iter 0 1 2))