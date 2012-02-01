(load "/home/barrucadu/code/scheme/generic.scm")

(define (trial-and-improvement f n acc verbose min max)
  (define (iter guess)
	(if (good-enough? guess) (average (car guess) (cdr guess))
		(iter (improve-guess guess))))

  (define (improve-guess guess)
	(if verbose (show-guess guess))
	(let ((mid (average (car guess) (cdr guess))))
	  (if (< (f mid) n)
		  (cons mid (cdr guess))
		  (cons (car guess) mid))))


  (define (good-enough? guess)
	(not (> (abs (- (cdr guess)
					(car guess)))
			(* 2 acc))))

  (define (show-guess guess)
	(display (car guess))
	(display " ")
	(display (cdr guess))
	(newline))

  (iter (cons min max)))

(define (improve f n)
  (trial-and-improvement f n 0.0001 #f -10000 10000))

(define (improve-verbose f n)
  (trial-and-improvement f n 0.0001 #t -10000 10000))
