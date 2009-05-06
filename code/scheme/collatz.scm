(load "/home/barrucadu/code/scheme/generic.scm")

(define (collatz n verbose)
  (define (iter i steps)
	(if verbose (show i))
	(cond ((= i 1) steps)
		  ((= (remainder i 2) 0) (iter (/ i 2) (+ steps 1)))
		  (else (iter (+ (* i 3) 1) (+ steps 1)))))
  (iter n 0))