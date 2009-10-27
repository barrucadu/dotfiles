;;;; Rule 30 is one of the 1-dimensional binary cellular automata.
;;;  000 -> x0x
;;;  001 -> x1x
;;;  010 -> x1x
;;;  011 -> x1x
;;;  100 -> x1x
;;;  101 -> x0x
;;;  110 -> x0x
;;;  111 -> x0x

(defun procstep (level)
  (let (retlist '())
	(dolist (cell level)
	  (if (and (equal (car cell) 0) (equal (car (cdr cell) 0)) (equal (car (cdr (cdr cell)) 0)))
		  (append retlist '(0)))
	  (if (and (equal (car cell) 0) (equal (car (cdr cell) 0)) (equal (car (cdr (cdr cell)) 1)))
		  (append retlist '(1)))
	  (if (and (equal (car cell) 0) (equal (car (cdr cell) 1)) (equal (car (cdr (cdr cell)) 0)))
		  (append retlist '(1)))
	  (if (and (equal (car cell) 0) (equal (car (cdr cell) 1)) (equal (car (cdr (cdr cell)) 1)))
		  (append retlist '(1)))
	  (if (and (equal (car cell) 1) (equal (car (cdr cell) 0)) (equal (car (cdr (cdr cell)) 0)))
		  (append retlist '(1)))
	  (if (and (equal (car cell) 1) (equal (car (cdr cell) 0)) (equal (car (cdr (cdr cell)) 1)))
		  (append retlist '(0)))
	  (if (and (equal (car cell) 1) (equal (car (cdr cell) 1)) (equal (car (cdr (cdr cell)) 0)))
		  (append retlist '(0)))
	  (if (and (equal (car cell) 1) (equal (car (cdr cell) 1)) (equal (car (cdr (cdr cell)) 1)))
		  (append retlist '(0))))))
