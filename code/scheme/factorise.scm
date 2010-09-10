(define (next-prime current)
  (define (prime? n cur)
    (if (or  (< n 2) (integer?      (/ n cur))  (> cur (sqrt n)))
        (and (> n 2) (not (integer? (/ n cur))) (> cur (sqrt n)))
        (prime? n (+ cur 1))))

  (if (or (= current 2) (prime? current 2)) current (next-prime (+ current 1))))

(define (blast-into-factors cur (prime 2) (factors '()))
  (cond ((= (next-prime cur) cur) (append factors (list cur)))
        ((integer? (/ cur prime)) (blast-into-factors (/ cur prime) prime (append factors (list prime))))
        (else                     (blast-into-factors cur (next-prime (+ prime 1)) factors))))
