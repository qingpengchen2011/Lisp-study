(define (make-account balance)
    (define (withdraw amount)
        (if (>= balance amount)
   	    (begin (set! balance (- balance amount))
                balance)
	    "Insufficient funds"))
    (define (desposit amount)
	(set! balance (+ balance amount))
	balance)
    (define (dispatch m)
        (cond ((eq? m 'withdraw) withdraw)
	      ((eq? m 'desposit) desposit)
	      (else (error "Unknown request --MAKE-ACCOUNT" m))))
    dispatch)
