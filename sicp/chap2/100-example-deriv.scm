(define (deriv expr var)
     (cond ((number? expr) 0)
           ((variable? expr)
              (if (same-variable? expr var) 1 0))
           ((sum? expr)
              (make-sum (deriv (addend expr) var)
                        (deriv (augend expr) var)))
           ;;102 page exercise 2.56
           ((exponentitation? expr)
              (let ((bs (base expr))
                    (expo (exponent expr)))
                   (make-product (make-product expo
					       (make-exponentiation bs (make-sum expo -1)))
				 (deriv bs var))))
                                      
           ((product? expr)
              (make-sum (make-product (multiplier expr)
                                      (deriv (multiplicand expr) var))
			(make-product (deriv (multiplier expr) var)
			              (multiplicand expr))))))
;;util
(define (=number? n1 n2)
    (and (number? n1) (= n1 n2)))
(define variable? symbol?)
(define (same-variable? v1 v2)
     (and (variable? v1) (variable? v2) (eq? v1 v2)))
;; expression:+
(define (sum? expr)
     (and (pair? expr)
          (eq? '+ (car expr))))
(define (make-sum a1 a2)
     (cond ((=number? a1 0) a2)
           ((=number? a2 0) a1)
           ((and (number? a1) (number? a2)) (+ a1 a2))
           (else 
              (list '+ a1 a2))))

(define (addend expr)
     (cadr expr))
(define (augend expr)
     (caddr expr))
;; expression:*
(define (product? expr)
    (and (pair? expr)
         (eq? '* (car expr))))
(define (make-product a1 a2)
    (cond ((or (=number? a1 0) (=number? a2 0)) 0)
          ((=number? a1 1) a2)
          ((=number? a2 1) a1)
          (else
             (list '* a1 a2))))
(define (multiplier expr)
    (cadr expr))
(define (multiplicand expr)
    (caddr expr))

;;102 page exercise 2.56
;;exponent
(define (make-exponentiation base exponent)
    (cond ((=number? base 0) 0)
          ((=number? exponent 0) 1)
          ((=number? exponent 1) base) 
          ((=number? base 1) 1)
          (else
            (list '** base exponent))))
(define (exponentitation? expr)
    (and (pair? expr) (eq? '** (car expr))))
(define (base expr)
    (cadr expr))
(define (exponent expr)
    (caddr expr))

