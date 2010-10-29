;; Useful functions
(define (^ base exponent)
  (exp (* exponent (log base))))

;; Reverse Polish Notation calculator
(define (rpn eqn)
  ; Char to symbol
  (define (char->symbol char)
    (if (c->f char) (c->f char)
        (c->i char)))

  ; ASCII char to integer
  (define (c->i char)
    (- (char->integer char) 48))
  
  ; Char to function
  (define (c->f char)
    (cond ((eq? #\+ char) +)
          ((eq? #\- char) -)
          ((eq? #\* char) *)
          ((eq? #\^ char) ^)
          ((eq? #\/ char) /)
          (else #f)))

  (define (rpncalc working subeqn)
    (if (= 0 (length subeqn)) working
        (rpncalc ((char->symbol (cadr subeqn))
                  working
                  (char->symbol (car subeqn)))
              (cddr subeqn))))

  (rpncalc (char->symbol (car (string->list eqn)))
           (cdr (string->list eqn))))