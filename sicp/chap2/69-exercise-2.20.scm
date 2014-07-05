(define (same-parity x . z)
    (define (inner o y)
        (cond ((null? y)
                 '())
              (  o
                 (if (even? (car y))
                     (cons (car y)
                           (inner o (cdr y)))
                     (inner o (cdr y))))
              (else
                 (if (odd? (car y))
                     (cons (car y) 
                           (inner o (cdr y)))
                     (inner o (cdr y))))))
    (inner (even? x) 
           (cons x z)))
              
                  
