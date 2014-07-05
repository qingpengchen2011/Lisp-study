(load "cont-frac.scm")

(define (tan-cf0 x k)
   (define N (lambda (i)
                     (if (= i 1)
                         x
                         (* x x))))
   (define D (lambda (i)
		      (- (* 2 i) 1)))
   (+ (cont-frac-recu-op N D k -)
      0.0))
(define (tan-cf1 x k)
   (define N (lambda (i)
                     (if (= i 1)
                         x
                         (* x x))))
   (define D (lambda (i)
		      (- (* 2 i) 1)))
                     
   (+ (cont-frac-iter-op N D K -)
      0.0))
