;;recursive
(define (cont-frac-recu-op n d k op)
    (define (cont-frac-inner i)
        (let ((ni (n i))
              (di (d i)))
             (if (> i k)
                 0
                 (/ ni 
                    (op di (cont-frac-inner (+ i 1)))))))
    (cont-frac-inner 1))

(define (gold-ratio0 k)
    (+ 1 
       (cont-frac-recu-op (lambda (x)
                            1.0)
                          (lambda (x)
                            1.0)
                          k
                          +)))

;;iterator
(define (cont-frac-iter-op n d k op)
  (define (cont-frac-it i result)
    ; (display i)
    ; (display " ,Result:")
    ; (display result)
    ; (newline)
     (if (= i 0)
         result
         (cont-frac-it (- i 1)    
                       (/ (n i)
                          (op (d i)
                             result)))))
  (cont-frac-it k 0))
(define (gold-ratio1 k)
   (+ 1
      (cont-frac-iter-op (lambda (x) 1.0)
                         (lambda (x) 1.0)
                         k
		         +)))

;; e approximate
  (define D  (lambda (x) 
                     (let ((r (remainder x 3)))
                          (cond ((= r 0) 1)
                                ((= r 1) 1)
                                (else (+ r (* 2
                                              (truncate (/ x 3)))))))))
(define (e0 k)
  (+ 2.0
     (cont-frac-recu-op (lambda (x) 1.0)
		        D
                        k
                        +)))
(define (e1 k)
  (+ 2.0 
     (cont-frac-iter-op (lambda (x) 1.0)
                        D
		        k
                        +)))
