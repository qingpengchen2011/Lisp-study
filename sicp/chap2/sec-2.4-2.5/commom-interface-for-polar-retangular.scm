(load "polar-package.scm")
(load "rectangular-package.scm")
(load "apply-generic.scm")

(install-polar-package)
(install-rectangular-package)
(define (myreal-part z) (apply-generic 'real-part z))
(define (myimag-part z) (apply-generic 'imag-part z))
(define (mymagnitue z) (apply-generic 'magnitude z))
(define (myangle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
     ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
     ((get 'make-from-mag-ang 'polar) r a))
