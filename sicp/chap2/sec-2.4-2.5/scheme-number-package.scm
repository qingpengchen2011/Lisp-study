(define (install-scheme-number-package)
    (load "tag-content.scm")
    (define (tag x) (attach-type 'scheme-number x))
    (put 'add '(scheme-number scheme-number)
   	         (lambda (x y) (tag (+ x y))))
    (put 'sub '(scheme-number scheme-number)
		 (lambda (x y) (tag (- x y))))
    (put 'mul '(scheme-number scheme-number)
		 (lambda (x y) (tag (* x y))))
    (put 'div '(scheme-number scheme-number)
		 (lambda (x y) (tag (/ x y))))
    (put 'make 'scheme-number
   	         (lambda (x) (tag x))) 
    
    ;;132-exercise-2.79
    (put 'equ? '(scheme-number scheme-number) (lambda (x y) (= x y)))
    ;;132-exercise-2.80
    (put 'zero? '(scheme-number) (lambda x) (= 0 x))
    'done)
