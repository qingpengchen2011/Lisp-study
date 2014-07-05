(define (myequal? a b)
    (let ((pa (pair? a))
          (pb (pair? b)))
         (cond ((and (not pa)
                     (not pb))
                  (eq? a b))
               ((and pa pb)
                  (and (myequal? (car a)
				 (car b))
                       (myequal? (cdr a)
                                 (cdr b))))
               (else
		  '#f))))