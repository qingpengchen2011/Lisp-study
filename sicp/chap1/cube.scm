(define (cube x)
  (define (improve old)
     (/ (+ (/ x (* old old))
           (* 2 old))
        3))
  (define (cube-iter new-guess old-guess)
     (define (goodenough? new old)
        (define (ratechange n o)
           (<  (/ (abs (- n o))
             	  o)
               0.0001))
        (ratechange new old))

     (if (goodenough? new-guess old-guess)
         old-guess
         (cube-iter (improve new-guess) new-guess)))
  (cube-iter (improve 1.0) 1.0))

   
