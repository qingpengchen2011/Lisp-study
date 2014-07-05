(load "apply-generic.scm")
(load "scheme-number-package.scm")
(load "rational-package.scm")
(load "complex-package.scm")

(install-scheme-number-package)
(install-rational-package)
(install-complex-package)

(define (add x y) (apply-generic 'add  x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

;;132-exercise-2.79
(define (equ? x y) (apply-generic 'equ? x y))

;;132-exercise-2.80
(define (=zero? x) apply-generic 'zero? x)
