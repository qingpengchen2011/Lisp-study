(define (loop? l)
    (define (it start next)
        (cond ((null? next) '#f)
              ((eq? start next) '#t)
              (else
                 (it start (cdr next)))))
    (it l (cdr l)))
    
