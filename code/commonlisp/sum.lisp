;;;; This is a really, horribly, ugly way to sum numbers. I wrote as much code as I could, to test my knowledge.

(defvar *tosum* (make-array 1 :fill-pointer 0 :adjustable t))

(defun clear-sum ()
  (setf *tosum* (make-array 1 :fill-pointer 0 :adjustable t)))

(defun add-to-sum (num)
  (vector-push-extend num *tosum*))

(defun calc-sum ()
  (reduce #'+ *tosum*))

(defun sum (&rest nums)
  (clear-sum)
  (dolist (num nums)
	(add-to-sum num))
  (format t "The sum is ~d" (calc-sum)))