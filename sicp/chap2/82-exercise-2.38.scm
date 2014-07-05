(load "accumulate.scm")
(define (myfold-left op init sequence)
    (define (iter result seq)
        (if (null? seq)
            result
            (iter (op result
                      (car seq))
                  (cdr seq))))
    (iter init sequence))

(define myfold-right accumulate)
