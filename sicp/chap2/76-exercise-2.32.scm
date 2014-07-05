;; empty set is saved in rest
(define (subsets s)
     (if (null? s)
         (list '())
         (let ((rest (subsets (cdr s))))
              (display rest)
              (newline)
              (append rest
                      (map (lambda (st)
                               (cons (car s) st))
                           rest))))) 
