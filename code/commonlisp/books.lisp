(defvar *db* nil)

(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))

(defun make-comparisons-list (fields)
  (loop while fields
		collecting (make-comparison-expr (pop fields) (pop fields))))

(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparisons-list clauses))))

(defun make-book (title author series subseries read position)
  (list :title title :author author :series series :subseries subseries :position position :read read))

(defun add-record (record)
  (push record *db*))

(defun dump-records (records)
  (dolist (cd records)
	(format t "~{~a:~10t~a~%~}~%" cd)))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-book ()
  (make-book
   (prompt-read "Title")
   (prompt-read "Author")
   (prompt-read "Series")
   (prompt-read "Subseries")
   (prompt-read "Position")
   (y-or-n-p "Read [y/n]: ")))

(defun add-books ()
  (loop (add-record (prompt-book))
		(if (not (y-or-n-p "Add another [y/n]: ")) (return))))

(defun save-db (filename)
  (with-open-file (out filename
					   :direction :output
					   :if-exists :supersede)
				  (with-standard-io-syntax
				   (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
				  (with-standard-io-syntax
				   (setf *db* (read in)))))

(defun select (selector-fn)
  (setf record (remove-if-not selector-fn *db*))
  (dump-records record))

(defun update (selector-fn &key title author series subseries position (read nil read-p))
  (setf *db*
		(mapcar
		 #'(lambda (row)
			 (when (funcall selector-fn row)
			   (if title (setf (getf row :title) title))
			   (if author (setf (getf row :author) author))
			   (if series (setf (getf row :series) series))
			   (if subseries (setf (getf row :subseries) subseries))
			   (if position (setf (getf row :position) position))
			   (if read-p (setf (getf row :read) read)))
			 row) *db*)))

(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))