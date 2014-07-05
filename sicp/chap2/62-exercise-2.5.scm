(define (mycons a b)
    (define (pow base n)
        (if (= n 0)
            1
            (* base (pow base (- n 1)))))
    (* (pow 2 a)
       (pow 3 b)))

(define (lg base c)
    (if (= c 1)
        0
        (+ 1 (lg base (/ c base)))))

(define (clear base n)
    (if (= 0
           (remainder n base))
        (clear base (/ n base))
        n))

(define (mycar c)
    (let ((n (clear 3 c)))
         (lg 2 n)))

(define (mycdr c)
    (let ((n (clear 2 c)))
         (lg 3 n)))
