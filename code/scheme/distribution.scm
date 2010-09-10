(load "/home/barrucadu/code/scheme/generic.scm")

;; Binomial Distribution functions
(define (make-binomial n p)
  (cons n p))

(define (get-binomial-trials b)
  (car b))

(define (get-binomial-prob b)
  (cdr b))

(define (binomial-prob b value)
  (let ((trials (get-binomial-trials b))
		(prob (get-binomial-prob b)))
	(* (choose trials
			   (- trials value))
	   (expt prob value)
	   (expt (- 1 prob)
			 (- trials value)))))

(define (binomial-prob-cumulative b value)
  (define (iter prob x)
	(if (> x value) prob
		(iter (+ prob (binomial-prob b x)) (+ x 1))))
  (iter 0 0))

(define (binomial-expected b)
  (* (get-binomial-trials b) (get-binomial-prob b)))

(define (binomial-variance b)
  (* (get-binomial-trials b)
	 (get-binomial-prob b)
	 (- 1 (get-binomial-prob b))))

;; Poisson Distribution functions
(define (make-poisson r)
  (cons r nil))

(define (get-poisson-rate p)
  (car p))

(define (poisson-prob p value)
  (/ (* (expt e (- (get-poisson-rate p)))
		(expt (get-poisson-rate p)
			  value))
	 (! value)))

(define (poisson-prob-cumulative p value)
  (define (iter prob x)
	(if (> x value) prob
		(iter (+ prob (poisson-prob p x)) (+ x 1))))
  (iter 0 0))
