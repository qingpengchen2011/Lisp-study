;;concept of sum
(define (sum a b term next)
   (if (> a b)
       0
       (+ (term a) (sum (next a) b term next))))

(define (inc n)
   (+ n 1))
(define (cube x)
   (* x x x ))

(define (sum-cube a b)
   (sum a b cube inc))

(define (sum-integer a b)
   (define (identify x)
      x)
   (sum a b identify inc))

(define (sum-pi a b)
   (define (term-pi x)
     (/ 1.0 (* x (+ x 2))))
   (define (term-next x)
     (+ x 4))
   (sum a b term-pi term-next))

;;integral
(define (integral f a b dx)
   (define (add-dx x) (+ x dx))
   (* (sum (+ a (/ dx 2))
           b 
           f 
           add-dx)
      dx))

;;precise-integral
(define (precise-integral f a b n)
   (define h
           (/ (- b a)
              n))

   (define (next x)
     (+ x 1))

   (define (term x)
     (define (factor k)
       (cond ((or (= k 0)
                  (= k n))
              1.0)
             ((even? k) 2.0)
             ((odd? k) 4.0))) 

     (define (yk k)
         (f (+ a (* k h))))

     (* (factor x)
        (yk x)))
   ;;call sum
   (* (sum 0 n term next)
      (/ h
          3.0)))

