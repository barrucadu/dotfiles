(load "/home/barrucadu/code/scheme/generic.scm")

(define (syntax-scheme)
  (show "All S-expressions will be on new lines, except in special cases.")
  (show "Indentation shall increase by two spaces every level.")
  (show "(define) expressions will have a blank line below them."))

(newline)
(syntax-scheme)
(newline)