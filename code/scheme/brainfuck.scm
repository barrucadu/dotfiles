(define (brainfuck code)
  (define (gen-brainfuck-cells size)
    (define (iter current cells)
      (if (= current size) cells
          (iter (+ current 1) (append cells '(0)))))
    (iter 0 '()))

  (define (get-brainfuck-cell cell cells)
    (define (iter current working)
      (if (= current cell) (car working)
          (iter (+ current 1) (cdr working))))
    (iter 0 cells))

  (define (set-brainfuck-cell cell cells)
    (define (iter current working)
      (if (= (+ current 1) cell) (car working)
          (iter (+ current 1) (cdr working))))
    (iter 0 cells))

  (define (iter codepos datapos cells size)
    (cond ((string=? (string-ref code codepos) ">")
           (if (>= datapos size) (iter (+ codepos 1) 0 cells size)
               (iter (+ codepos 1) (+ datapos 1) cells size)))
          ((string=? (string-ref code codepos) "<")
           (if (<= datapos 0) (iter (+ codepos 1) datapos cells size)
               (iter (+ codepos 1) (- datapos 1) cells size)))
          ((string=? (string-ref code codepos) "+")
           (if (<= datapos 0) (iter (+ codepos 1) datapos cells size)
               (iter (+ codepos 1) (- datapos 1) cells size)))


  (iter 0 0 (gen-brainfuck-cells 30000) 30000))
