;;;; Generic functions
;; Include into other files with (load "/home/barrucadu/code/scheme/generic.scm")

;;;;;;;;;; Variable Declarations
(define nil '())
(define pi  3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679)
(define e   2.7182818284590452353602874713526624977572470936999595749669676277240766303535475945713821785251664274)
(define phi 1.6180339887498948482045868343656381177203091798057628621354486227052604628189024497072072041893911374)

;;;;;;;;;; Basic Variable Functions
(define (show x)
  (display x)
  (newline))

(define (identity x)
  x)

;;;;;;;;;; Simple Mathematical Functions
(define (inc x)
  (+ x 1))

(define (dec x)
  (- x 1))

(define (! n)
  (prod 1 n))

(define (gcd a b)
  (if (= b 0) a
	(gcd b (remainder a b))))

(define (average x y)
  (/ (+ x y) 2))

(define (cube n)
  (* n n n))

(define (square n)
  (* n n))

(define (choose n k)
  (if (or (< k 0) (> k n)) 0
	  (/ (! n)
		 (* (! k)
			(! (- n k))))))

(define (isprime n)
  (define (iter cur)
	(cond ((= (remainder n cur) 0) #f)
		  ((<= cur (sqrt n)) (iter (+ cur 1)))
		  (else #t)))
  (cond ((= n 2) #t)
		((< n 2) #f)
		(else (iter 2))))

;;;;;;;;;; Numerical Accumulation Functions
(define (accumulator from to increment counter function)
  (define (iter current total)
	(if (= current to) total
		(let ((newcurrent (increment current)))
		  (iter newcurrent
				(counter total (function newcurrent))))))
  (if (or (= from to) (< to from)) (function from)
		(iter from (function from))))

(define (sum from to)
  (accumulator from to inc
			   (lambda (x y) (+ x y))
			   identity))

(define (prod from to)
  (accumulator from to inc
			   (lambda (x y) (let ((z (* x y)))
							   (if (= z 0) 1 z)))
			   identity))

(define (exp-func x r)
  (+ 1 (accumulator 0 r inc
			   (lambda (a b) (+ a (if (= b 0) 0 (/ (expt x b) (! b)))))
			   identity)))

;;;;;;;;;; Exponention
(define (exp x)
  (exp-func x 200))

(define e
  (exp-func 1.0 200))

;;;;;;;;;; Function-related Functions
(define (doublefunc f)
  (lambda (x) (f (f x))))

(define (triplefunc f)
  (lambda (x) (f (f (f x)))))

;;;;;;;;;; Trigonometric Functions
(define (deg-to-rad angle)
  (* angle (/ pi 180)))

(define (rad-to-deg angle)
  (/ angle (/ pi 180)))

(define (sin-deg angle)
  (sin (deg-to-rad angle)))

(define (cos-deg angle)
  (cos (deg-to-rad angle)))

(define (tan-deg angle)
  (tan (deg-to-rad angle)))

(define (asin-deg num)
  (rad-to-deg (asin num)))

(define (acos-deg num)
  (rad-to-deg (acos num)))

(define (atan-deg num)
  (rad-to-deg (atan num)))

(define (csc angle)
  (/ 1 (sin angle)))

(define (sec angle)
  (/ 1 (cos angle)))

(define (cot angle)
  (/ 1 (tan angle)))

(define (csc-deg angle)
  (/ 1 (sin-deg angle)))

(define (sec-deg angle)
  (/ 1 (cos-deg angle)))

(define (cot-deg angle)
  (/ 1 (tan-deg angle)))

;;;;;;;;;; Predicate Functions
(define (<> a b)
  (or (< a b) (> a b)))

;;;;;;;;;; List Functions
(define (combine-lists lista listb)
  (if (null? lista)
	  listb
	  (cons (car lista) (combine-lists (cdr lista) listb))))

;;;;;;;;;; Misc Functions
(define (hanoi levels)
  (- (expt 2 levels) 1))
