(define (deep-reverse l)
    (define (inner result left)
       (cond ((null? left)
               result)
             (else
                 (inner
                     (cons (if (not (pair? (car left)))
                               (car left)
                               (inner '()
                                       (car left)))
                            result)
                     (cdr left)))))
    (inner '() l))
       
;;an ugly version

(define (ugly-deep-reverse l)
    (define (inner result left)
        (cond ((null? left)
               result)
              (else
                (let ((head (car left))
                      (tail (cdr left)))
                     (cond ((not (pair? head))
                            (inner (cons head result)
                                   tail))
                           (else
                               (inner (cons (inner '() head) 
                                            result)
                                      tail)))))))
    (inner '() l))

