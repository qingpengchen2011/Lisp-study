(define (square x)
    (* x x))

(define (square-tree0 tree)
    (cond ((null? tree) 
            '())
          ((not (pair? tree))
            (square tree))
          (else 
             (cons (square-tree0 (car tree))
                   (square-tree0 (cdr tree))))))

(define (square-tree1 tree)
    (map (lambda (sub-tree)
             (if (pair? sub-tree)
                 (square-tree1 sub-tree)
                 (square sub-tree)))
         tree))
