(load "/home/barrucadu/code/scheme/generic.scm")

(define *todo* nil)

(define (make-entry description priority deadline)
  (list description priority deadline))

(define (add-entry entry)
  (set! *todo* (append *todo* entry)))

(define (search term)
  (define (iter todo)
	(cond ((memq term todo) (show todo))
		  ((<> nil todo) (iter (cdr todo)))))
  (iter *todo*))

(define (save-list path)
  