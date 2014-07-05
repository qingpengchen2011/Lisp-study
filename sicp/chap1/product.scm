;;recusive version
(define (product-recu a b term next)
   (if (> a b)
       1
       (* (term a)
          (product-recu (next a)
                        b
                        term
                        next))))

;;iterator version
;;invariant: a*b*result
(define (product-iter a b term next)
   (define (product-iter-inner a result)
      (if (> a b)
          result
          (product-iter-inner (next a) 
                              (* (term a) result))))
   (product-iter-inner a 1))

(define (factorial-recu n)
   (define (term a) a)
   
   (define (next a) (+ a 1))
  
   (product-recu 1 n term next))

(define (factorial-iter n)
   (define (term a) a)
   
   (define (next a) (+ a 1))

   (product-iter 1 n term next))

;;PI
(define (pi n)
   (define (term x) 
      (/ (* (- x 1)
            (+ x 1))
         (square x)))
   (define (next x)
      (+ x 2))
   (* 4.0
      (product-iter 3 n term next)))

;;higher abstract than sum and product (recursive version)
(define (accumulate-recu combiner null-value term a next b)
   (if (> a b)
       null-value
       (combiner (term a)
                 (accumulate-recu combiner
			          null-value
                                  term
				  (next a)
                                  next 
				  b))))
;;(iterator version)
(define (accumulate-iter combiner null-value term a next b)
   (define (accumulate-iter-inner a result)
     (if (> a b)
         result
         (accumulate-iter-inner (next a)
                                (combiner (term a)
    					  result))))
   (accumulate-iter-inner a null-value))

;;define product and  with accumulate abstract
(define (acc-product term a next b)
   (accumulate-iter * 1 term a next b))

(define (acc-sum term a next b)
   (accumulate-iter + 0 term a next b))

;;add filter to accumulate recursive
(define (filtered-accumulate-recu combiner null-value term a next b valid?)
    (if (> a b)
        null-value
        (if (valid? a)
            (combiner (term a)
                      (filtered-accumulate-recu combiner
					        null-value
					        term
					        (next a)
					        next
					        b
					        valid?))
            (filtered-accumulate-recu combiner
                                      null-value
                                      term
				      (next a)
                                      next 
				      b
				      valid?))))
;;iterator version
(define (filtered-accumulate-iter combiner null-value term a next b valid?)
   (define (filtered-accumulate-iter-inner a result)
        (if (> a b)
            result
            (if (valid? a)
                (filtered-accumulate-iter-inner (next a)
                                                (combiner (term a)
						          result))
                (filtered-accumulate-iter-inner (next a)
                                                result))))
   (filtered-accumulate-iter-inner a null-value))

