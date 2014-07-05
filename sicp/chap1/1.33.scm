(load "gcd.scm")
(load "product.scm")
(load "prim.scm")

(define (primes-sum a b)
   (define (term x) x)

   (define (next x) (+ x 1))

   (filtered-accumulate-recu  +
			      0
			      term
			      a
                              next
			      b
                              prime?)) 

(define (product-of-coprimes n)
   (define (coprime? i)
     (and (< i n)
          (= (gcd i n) 
             1)))

   (define (term x) x)

   (define (next x) (+ x 1))

   (filtered-accumulate-iter * 
                             1
			     term
                             1
			     next
			     n
                             coprime?))
