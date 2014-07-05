(define (ugly-fringe l)
    (cond ((null? l)
           '())
          (else
            (let ((head (car l))
                  (tail (cdr l)))
                 (cond ((not (pair? head))
                         (cons head (fringe tail)))
                       (else 
                          (append (fringe head)
                                (fringe tail))))))))

(define (fringe l)
    (cond ((null? l)
            '())
          ((not (pair? l))
            (list l))
          (else
              (append (fringe (car l))
                      (fringe (cdr l)))))) 
