(define (square x)
   (* x x))


;;define a*b^n as the invariant
(define (fast-expt1 b n)
    (define (fast-expt-iter b n a)
        (cond ((= n 0) a)
              ((even? n) (fast-expt-iter (square b) (/ n 2) a))
              (else (fast-expt-iter b (- n 1) (* a b)))))

    (fast-expt-iter b n 1))
   

;;recursive
(define (fast-expt0 b n)
   (cond ((= n 0) 1)
         ((even? n) (square (fast-expt0 b (/ n 2))))
         (else (* b (fast-expt0 b (- n 1))))))
