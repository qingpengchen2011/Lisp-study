(define (count-pairs x)
    (let ((visited '()))
         (define (in-visited? test visited-list)
                 (cond ((null? visited-list) '#f)
                       ((eq? test (car visited-list)) '#t)
                       (else
                           (in-visited? test (cdr visited-list)))))
         (define (inner x)
                 (if (not (pair? x))
                     0
                     (begin (set! visited (cons x visited))
                            (let ((head (car x))
                                  (next (cdr x)))
                                 (+ 1 (if (in-visited? head visited)
                                          0
					  (inner head))
                                      (if (in-visited? next visited)
					  0
					  (inner next)))))))
 	  (inner x)))

;;better version
(define (count-pairs x)
    (length (inner x '())))

(define (inner x memo-list)
    (if (and (pair? x)
             (false? (memq x memo-list)))
        (inner (car x)
               (inner (cdr x)
                      (cons x memo-list)))
        memo-list))

;;from book, error version
(define (count-pairs0 x)
    (if (not (pair? x))
	0
	(+ 1 (count-pairs0 (car x))
	     (count-pairs0 (cdr x)))))                 

(define three (list 'a 'b 'c))

(define a (list 'a))
(define four (cons a (cons a '())))

(define one (list 'b))
(define three1 (cons one one ))
(define seven (cons three1 three1))
