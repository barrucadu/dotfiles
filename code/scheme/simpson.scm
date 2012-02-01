(define (simpson f a b n)
  (define (iter ans m dx x i)
    (if (= i n) ans
	(iter
	 (+ ans
	    (* m
	       (f x)))
	 (- 6 m)
	 dx
	 (+ x dx)
	 (+ i 1))))
  
  (let ((dx (/
	     (- b a)
	     n))
	(ans (+
	      (f a)
	      (f b)))
	(x (+ a
	      (/
	       (- b a)
	       n))))
    (/ (*
	(iter ans 4 dx x 1)
	dx)
       3)))
