(define (make-rat n d)
  (let ((g (gcd n d)))
       (cond ( (> d 0)
               (cons (/ n g)
                     (/ d g)))
             (else (cons (/ (- n) g)
                         (/ (- d) g)))))) 
            
