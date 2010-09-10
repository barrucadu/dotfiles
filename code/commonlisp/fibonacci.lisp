;;;; This file contains various functions to calculate the Fibonacci Sequence up to an arbitrary end point, by default 10.

;; Here is my first implementation, an ugly DOTIMES loop with a load of variables.
(defun fib (&optional (end 10))
  (setf n1 1)
  (setf n2 1)
  (dotimes (i end)
	(print n1)
	(setf n3 (+ n1 n2))
	(setf n1 n2)
	(setf n2 n3)))

;; Here is my second implementation, a much nicer DO loop with variables defined in the loop itself.
(defun fib-do (&optional (end 10))
  (do ((i 0 (incf i)) (n1 1 n2) (n2 1 n3))
	  ((equal i end))
	(print n1)
	(setf n3 (+ n1 n2))))