(load "/home/barrucadu/code/scheme/generic.scm")

;; Sort a list by using the bubble sort algorithm, comparing elements with the
;; specified function. This should return #t if the values are in the correct
;; order, and any other value otherwise.
(define (bubblesort list comparison)
  (define (swap items)
	(if (= (comparison (car items) (cdar items)) #t)
		items
		(list (cdar items) (car items))))

  (define (sort output step)
	(if (null? (cdr step))
		output
		(let ((sorted (swap step)))
		  (sort (combine-lists current sorted) (list (cdr sorted) (

  (define (iterator current)
	(if (= current (sort current))
		current
		(iterator (sort current))))
  
  (iterator list))