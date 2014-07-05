(load "../chap1/prim.scm")
(define (stream-enumerate-interval low high)
    (if (> low high)
        the-empty-stream
        (cons-stream low (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
    (cond ((stream-null? stream) the-empty-stream)
	  ((pred (stream-car stream))
		(cons-stream (stream-car stream)
			     (stream-filter pred (stream-cdr stream))))
	  (else
		(stream-filter pred (stream-cdr stream)))))
(define (stream-for-each proc s)
     (if (stream-null? s)
         'done
	 (begin (proc (stream-car s))
		(stream-for-each proc (stream-cdr s)))))
(define (display-stream s)
    (stream-for-each display-line s))

(define (display-line x)
    (newline)
    (display x)) 

