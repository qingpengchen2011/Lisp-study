(load "accumulate.scm")
(load "accumulate-n.scm")
(define (dot-product v w)
     (accumulate + 0 (map * 
                         v 
			 w)))

(define (matrix-*-vector m v)
    (map (lambda (sub-v)
                 (dot-product sub-v v))
         m))

(define (transpose mat)
    (accumulate-n (lambda (x y)
                          (cons x y))
                  '()
                   mat))

(define (matrix-*-matrix m n)
    (let ((cols (transpose n)))
         (map (lambda (row)
                  (matrix-*-vector cols row))
              m)))
