(define (next-prime current)
  (define (siter n cur)
    (cond ((= (remainder n cur) 0) #f)
          ((<= cur (sqrt n)) (siter n (+ cur 1)))
          (else #t)))

  (define (iter cur)
    (if (cond ((= cur 2) #t)
              ((< cur 2) #f)
              (else (siter cur 2)))
        cur
        (iter (+ cur 1))))

  (iter (+ current 1)))

(define (blast-into-factors x)
  (define (iter cur prime factors)
    (cond ((= (next-prime (- cur 2)) cur) (append factors (list cur)))
          ((integer? (/ cur prime))       (iter (/ cur prime) prime (append factors (list prime))))
          (else                           (iter cur (next-prime prime) factors))))

  (iter x 2 '()))
