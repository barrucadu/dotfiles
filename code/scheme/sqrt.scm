(load "/home/barrucadu/code/scheme/generic.scm")

(define (sqrt x)
  (define (sqrt-iter guess x)
	(if (good-enough? guess x)
		guess
	  (sqrt-iter (improve guess x)
				 x)))

  (define (improve guess x)
	(average guess (/ x guess)))

  (define (average x y)
	(/ (+ x y) 2))

  (sqrt-iter 1.0 x))