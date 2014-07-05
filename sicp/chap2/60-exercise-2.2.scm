;;representation of point
(define (make-point x y)
    (cons x y))

(define (x-point p)
    (car p))

(define (y-point p)
    (cdr p))

;;representation of segment
(define (make-segment sp ep)
    (cons sp ep))

(define (start-segment seg)
    (car seg))

(define (end-segment seg)
    (cdr seg))

;;usage
(define (midpoint-segment seg)
    (let ((sp (start-segment seg))
          (ep (end-segment seg)))
         (let ((x1 (x-point sp))
               (x2 (x-point ep))
               (y1 (y-point sp))
               (y2 (y-point ep)))
              (make-point (/ (+ x1 x2) 2)
                          (/ (+ y1 y2) 2))))) 

(define (print-point p)
    (newline)
    (display "(")
    (display (x-point p))
    (display ",")
    (display (y-point p))
    (display ")"))
