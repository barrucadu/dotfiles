(require 'sb-posix)

;; The current working directory
(defconstant *cwd* (parse-namestring (concatenate 'string (sb-posix:getcwd) "/")))

;; The argument list and count.  *ARGV-RAW* is the unmolested values,
;; whereas *ARGV* has everything converted to absolute paths.
(defconstant *argv-raw* (cdr sb-ext:*posix-argv*)) ; first argument is "sbcl", throw that away.
(defconstant *argn*     (length *argv-raw*))
(defconstant *argv*     (mapcar #'(lambda (arg) (merge-pathnames (parse-namestring arg) *cwd*)) *argv-raw*))

;; The list of all rules.  Populated by RULE.
(defparameter *rules* nil)

;; The current rule priority.  Set by RULE.  Only matching rules of at
;; least this priority are added to *RULES*.
(defparameter *rule-priority* 0)

;; Stringify every element in a list with FORMAT.
(defun stringify-list (xs)
  (mapcar #'(lambda (x) (format nil "~A" x)) xs))

;; Start a process and return a stdin/stdout stream.
(defun stream-process-in-out (cmd &optional args)
  (let* ((p (sb-ext:run-program cmd (stringify-list args)
                                :input  :stream
                                :error  nil
                                :output :stream
                                :wait   nil
                                :search t))
         (s (make-two-way-stream (sb-ext:process-output p)
                                 (sb-ext:process-input  p))))
    (cons p s)))

;; Start a process with STREAM-PROCESS-IN-OUT and immediately close
;; the stdin stream.
(defun stream-process-out (cmd &optional args)
  (let ((ps (stream-process-in-out cmd args)))
    (finish-output (cdr ps))
    ps))

;; Run a process and return the output and exit code.
(defun system (cmd &optional args)
  (let* ((ps (stream-process-out cmd args))
         (p  (car ps))
         (s  (cdr ps))
         (out (with-output-to-string (o)
                (loop for line = (read-line s nil nil)
                      while line
                      do (write-line line o)))))
    (close s)
    (sb-ext:process-wait p)
    (cons (string-right-trim '(#\Newline) out) (sb-ext:process-exit-code p))))

;; Run a process and return its output.
(defun system-1 (cmd &optional args)
  (car (system cmd args)))

;; Run a process and return its exit code.
(defun system-2 (cmd &optional args)
  (cdr (system cmd args)))

;; Concatenate strings.
(defun concat (&rest rest)
  (apply #'concatenate 'string rest))

;; Split a string on a delimiter.
(defun split-on (string delim &optional tail)
  (let ((idx (position delim string :from-end t :test (lambda (x y) (find y x :test #'string=)))))
    (if idx
      (split-on (subseq string 0 idx) delim (cons (subseq string (+ 1 idx)) tail))
      (cons string tail))))

;; Define a new rule.
(defun defrule (name p cmds &key (priority 0))
  (when (and p (not (null cmds)))
    (let ((the-rule (list name cmds)))
      (cond ((> priority *rule-priority*)
             (setf *rules* (list the-rule))
             (setf *rule-priority* priority))
            ((eq priority *rule-priority*)
             (setf *rules* (cons the-rule *rules*)))))))

;; Run a command
(defun do-cmd (cmd &key pre-args (args *argv-raw*) post-args)
  (sb-ext:run-program cmd (stringify-list (append pre-args args post-args))
                      :input  t
                      :error  t
                      :output t
                      :wait   t
                      :search t))

;; Run a rule
(defun do-rule (name cmds)
  (dolist (cmd cmds)
    (apply #'do-cmd cmd)))

;; Pretty-print a command
(defun format-cmd (cmd &key pre-args (args *argv-raw*) post-args dest)
  (format dest "~A (~{~A~^, ~})" cmd (append pre-args args post-args)))

;; Pretty-print a rule
(defun format-rule (name cmds &key (dest *standard-output*))
  (format dest "~A: ~{~A~%~^... ~}" name (mapcar (lambda (cmd) (apply #'format-cmd cmd)) cmds)))

;; Select the rule to run
(defun select-rule ()
  (let ((rules (reverse *rules*)))
    (cond ((null rules) (format t "There are no matching rules!~%"))
          ((null (cdr rules)) (apply #'do-rule (car rules)))
          (t (let ((i 0))
               (dolist (rule rules)
                 (format t "[~A] " i)
                 (apply #'format-rule rule)
                 (setf i (+ i 1))))
             (format t "Multiple rules match, enter a number: ")
             (finish-output)
             (let ((choice (read)))
               (if (typep choice 'integer)
                   (if (and (>= choice 0) (< choice (length rules)))
                       (apply #'do-rule (nth choice rules))
                     (format t "Invalid choice.  Aborting.~%"))
                 (format t "Aborting.~%")))))))
