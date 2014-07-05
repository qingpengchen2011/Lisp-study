(load "newtons.scm")
;exec-1.40
(define (cubic_abc a b c)
    (newtons-method (cubic a b c) 1))

(define (cubic a b c)
    (lambda (x)
            (+ (* x x x)
               (* a x x)
               (* b x)
               c)))
;;exec-1.42
(define (compose f g)
    (lambda (x)
            (f (g x))))
;;exec-1.43
(define (repeated f n)
    (if (= n 1)
        f
        (compose f (repeated f (- n 1)))))
            
;;exec-1.44

(define (f-smooth f)
    (define dx 0.00001)
    (lambda (x)
            (/ (+ (f (- x dx))
                  (f x)
                  (f (+ x dx)))
               3)))

(define (f-n-smooth f n)
    ((repeated f-smooth n) f))

;;exec-1.45

(load "fast-expt.scm")
(load "newtons.scm")
(define (nth-root x n)
    (define (real-fun y)
        (/ x (fast-expt1 y
                         (- n 1))))
    (define (transform k)
        (repeated average-damp k))
    (let ((k (floor (/ (log n)
                       (log 2)))))
        (fixed-point-transform (transform k) 
                               real-fun 
                               1.0)))
    
  

;;exec-1.46 
;; see iterative-improve.scm
