(load "82-exercise-2.38.scm")

(define (reverse0 sequence)
    (myfold-right (lambda (x y)
                          (append y
                                  (list x)))
                  '()
                  sequence))

(define (reverse1 sequence)
    (myfold-left (lambda (x y)
                         (cons y x))
                 '()
                 sequence))


