(load "/home/barrucadu/code/scheme/generic.scm")

(define (expand-binomial x y n)
  (define (iter latex k)
	(if (> k n) (string-append "$$" latex "$$")
		(if (<> k 0)
			(iter (string-append latex "+ {"
								 (number->string n) " \\choose "
								 (number->string k) "}{" x "}^{"
								 (number->string n) " - "
								 (number->string k) "}{" y "}^{"
								 (number->string k) "}")
				  (inc k))
			(iter (string-append latex "{"
								 (number->string n) " \\choose "
								 (number->string k) "}{" x "}^{"
								 (number->string n) " - "
								 (number->string k) "}{" y "}^{"
								 (number->string k) "}")
				  (inc k)))))
  (iter "" 0))