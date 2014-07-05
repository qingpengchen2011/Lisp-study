(load "stream.scm")

(define (add-stream s1 s2)
    (stream-map + s1 s2))

(define ones (cons-stream 1 ones))

(define (intergers-starting-from n)
     (cons-stream n (intergers-starting-from (+ n 1))))

(define intergers (cons-stream 1 (add-stream ones intergers)))
