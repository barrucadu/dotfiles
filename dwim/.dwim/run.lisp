(load (merge-pathnames #P".dwim/rules.lisp" (user-homedir-pathname)))

;; Ignore INTERACTIVE-INTERRUPT (control C).
(handler-case (select-rule)
  (sb-sys:interactive-interrupt ()
   (sb-ext:quit)))
