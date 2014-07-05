(define (tree-map0 f tree)
    (map (lambda (sub-tree)
             (if (pair? sub-tree)
                 (tree-map0 f sub-tree)
                 (f sub-tree)))
         tree))

(define (tree-map1 f tree)
    (cond ((null? tree)
            '())
          ((not (pair? tree))
           (f tree))
          (else
             (cons (tree-map1 f (car tree))
                   (tree-map1 f (cdr tree))))))
     

(define (square x) (* x x ))

(define (square-tree0 tree)
    (tree-map0 square tree))

(define (square-tree1 tree)
    (tree-map1 square tree))


