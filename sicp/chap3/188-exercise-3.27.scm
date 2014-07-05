(load "1table.scm")
(define (memoize f) 
    (let ((table (make-1table)))
        (lambda (x)
            (let ((previous-result (lookup x table)))
		 (or previous-result
		     (let ((result (f x)))
			  (insert! x result table)
			  result))))))
   

(define mem-fib (memoize (lambda (x)
				 (cond ((= x 0) 0)
				       ((= x 1) 1)
				       (else 
					  (+ (mem-fib (- x 1))
				             (mem-fib (- x 2))))))))
