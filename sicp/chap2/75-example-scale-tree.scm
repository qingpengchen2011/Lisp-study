(define (scare-tree0 tree factor)
    (cond ((null? tree)
            '()) 
          ((not (pair? tree))
            (* tree factor))
          (else
             (cons (scare-tree0 (car tree) factor)
                   (scare-tree0 (cdr tree) factor)))))

(define (scare-tree1 tree factor)
    (map (lambda (sub-tree)
              (if (pair? sub-tree)
                  (scare-tree1 sub-tree factor)
                  (* factor sub-tree)))
         tree))

