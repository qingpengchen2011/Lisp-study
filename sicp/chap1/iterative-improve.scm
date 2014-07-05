(define (average-damp f)
   (lambda (x)
      (average x (f x))))
(define (good-enough? a b)
    (< (abs (- b a))
       0.000001))

(define (average a b)
    (/ (+ a b)
       2))



;;exercise 1.46
;;high level abstraction 
(define (iterative-improve good-enough? improve-f)
    (lambda (first-guess)
        (define (display-info guess step)
            (display "Step: ")
            (display step)
            (display ",Guess:")
            (display guess)
            (newline))
        (define (try-it guess step)
            (display-info guess step)
            (let ((next-guess (improve-f guess)))
                 (cond ((good-enough? guess next-guess)
                           (display-info next-guess (+ step 1))
                           next-guess)
                       (else
                           (try-it next-guess (+ 1 step))))))
        (try-it first-guess 1)))

;;using iterative-improve
(define (my_sqrt x)
    (let ((f (iterative-improve good-enough? 
                                (average-damp (lambda (y)
                                                      (/ x y))))))
         (f 1.0)))
                         
(define (my_fixed-point g first-guess)
    (let ((f (iterative-improve good-enough?
                                g)))
         (f first-guess)))

