(define (sum-deriv operands var)
    (make-sum (deriv (car operands) var)
	      (deriv (cadr operands) var)))
;;install
(put 'deriv '+ sum-deriv)

(define (product-deriv operands var)
    (make-sum
	(make-product (car operands)
		      (deriv (cadr operands) var))
	(make-product (deriv (car operands) var)
		      (cadr operands))))

;;install
(put 'deriv '* product-deriv)
