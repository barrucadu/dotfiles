;; Exercise 1.11.  A function f is defined by the rule that f(n) = n if n<3 and f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n>=3. Write a procedure that computes f by means of a recursive process. Write a procedure that computes f by means of an iterative process.

(define (f-recursive n)
  (if (< n 3) n
	  (+ (f-recursive (- n 1)) (* 2 (f-recursive (- n 2))) (* 3 (f-recursive (- n 3))))))