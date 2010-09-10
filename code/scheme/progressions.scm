;; Arithmatic Progression
(define (make-arithmatic a d)
  (cons a d))

(define (get-arithmatic-first p)
  (car p))

(define (get-arithmatic-difference p)
  (cdr p))

(define (arithmatic-term p n)
  (+ (get-arithmatic-first p)
	 (* (get-arithmatic-difference p)
		(- n 1))))

;; Geometric Progression
(define (make-geometric a r)
  (cons a r))

(define (get-geometric-first p)
  (car p))

(define (get-geometric-ratio p)
  (cdr p))

(define (geometric-term p n)
  (* (get-geometric-first p)
	 (expt (get-geometric-ratio p)
		   (- n 1))))