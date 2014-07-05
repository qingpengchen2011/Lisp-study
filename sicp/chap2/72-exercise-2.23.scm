(define (foreach f l)
    (cond ((null? l)
           '#t)
          (else
              (f (car l))
              (foreach f (cdr l)))))
        
