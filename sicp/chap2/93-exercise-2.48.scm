(define (make-segment vect1 vect2)
    (list vect1 vect2))

(define (start-segment seg)
    (car seg))

(define (end-segment seg)
   (cadr seg))
