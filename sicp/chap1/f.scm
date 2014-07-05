
;;;recursive
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))
;;;iterator
(define (f-iter n)
  (define (dof a b c count)
    (if (> count n)
        c
        (dof b 
	     c 
             (+ c (* 2 b)
                  (* 3 a))
             (+ 1 count))))
  (if (< n 3)
      n
      (dof 0 1 2 3)))
