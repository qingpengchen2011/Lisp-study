(load "225-exercise-3.50.scm")

(define (show x)
    (display-line x)
    x)

(define x (stream-map show (stream-enumerate-interval 0 10)))


