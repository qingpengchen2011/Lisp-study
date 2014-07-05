(define (zero-poly poly)
    (define (zero-termlist termlist)
        (if (null? termlist)
	    '#f
            (and (=zero? (coeff (car termlist)))
                 (zero-termlist (cdr termlist)))))
    (zero-termlist (term-list poly)))

(put '=zero? '(polynomial) zero-poly)
