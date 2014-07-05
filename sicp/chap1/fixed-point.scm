(define (closeenough? a b)
    (< (abs (- b a))
       0.000001))

(define (average a b)
    (/ (+ a b)
       2))

(define (search f neg-point pos-point)
    (let ((mid-point (average neg-point pos-point)))
         (cond ((closeenough? neg-point pos-point) mid-point)
               (else (let ((mid-value (f mid-point)))
                          (cond ((positive? mid-value) (search f neg-point mid-point))
                                ((negative? mid-value) (search f mid-point pos-point))
                                (else mid-point)))))))

(define (half-interval-method f a b)
    (let ((a-value (f a))
          (b-value (f b)))
         (cond ((and (positive? a-value)
                     (negative? b-value))
                (search f b a))
               ((and (negative? a-value)
                     (positive? b-value))
                (search f a b))
               (else (error "invalid a b" a b)))))

;;another method;
(define (fixed-point f first-guess)
    (define (display-info guess step)
      (display "Step: ")
      (display step)
      (display " ,Guess:")
      (display guess)
      (newline))
    (define (try-guess guess step)
      (display-info guess step)
      (let ((next-guess (f guess)))
           (cond ((closeenough? guess next-guess) 
                    (display-info next-guess (+ step 1))
                    next-guess)
		 (else 
                    (try-guess next-guess (+ step 1))))))
    (try-guess first-guess 1))                 
