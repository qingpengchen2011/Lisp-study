(load "average.scm")
(load "fixed-point.scm")

(define (average-damp f)
   (lambda (x)
      (average x (f x))))

(define dx 0.00001)
(define (devi g)
    (lambda (x)
      (/ (- (g (+ x dx))
             (g x))
         dx)))
;;using newthon method to compute a fixed point of the new generated function f
(define (newton-transform g)
    (lambda (x)
        (- x (/ (g x)
                ((devi g) x)))))
;;using fixed-point to compute the new function f to get the fixed point 
(define (newtons-method g guess)
    (fixed-point (newton-transform g) guess))

;; a higher level abstraction of fixed point
(define (fixed-point-transform transform g first-guess)
    (fixed-point (transform g) first-guess))

(define (mysqrt0 x)
    (fixed-point-transform average-damp 
                           (lambda (y) 
                                   (/ x y))
                           1.0))

(define (mysqrt1 x)
    (fixed-point-transform newton-transform
		           (lambda (y)
                                   (- x (* y y)))
                           1.0))


