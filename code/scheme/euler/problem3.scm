;; Problem 3
;; What is the largest prime factor of the number 600851475143 ?

(load "/home/barrucadu/code/scheme/generic.scm")

(define (euler3 n)
  (define (iter cur prime)
	(cond ((> cur (sqrt n)) prime)
		  ((and (isprime cur) (= (remainder n cur) 0)) (iter (+ cur 1) cur))
		  (else (iter (+ cur 1) prime))))
  (if (isprime n) n
	  (iter 2 0)))