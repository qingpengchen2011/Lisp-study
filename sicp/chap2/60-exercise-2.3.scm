(load "60-exercise-2.3.scm")
;;general representation
(define (make-rec len wid)
    (cons len wid))

(define (rec-length r)
    (car r))

(define (rec-width r)
    (cdr r))

;;segmenet representation
(define (rec-length r)
    (let ((seg (car r)))
         (let ((sp (start-point seg))
               (ep (end-point seg)))
              (let ((x1 (x-point sp))
                    (y1 (y-point sp))
                    (x2 (x-point ep))
                    (y2 (y-point ep)))
                   (sqrt (+ (* (- x1 x2)
                               (- x1 x2))
                            (* (- y1 y2)
                               (- y1 y2))))))))

(define (rec-width r)
    (let ((seg (cdr r)))
         (let ((sp (start-point r))
               (ep (end-point r)))
              (let ((x1 (x-point sp))
                    (y1 (y-point ep))
                    (x2 (x-point sp))
                    (y2 (y-point ep)))
                   (sqrt (+ (* (- x1 y1)
                               (- x1 y1))
                            (* (- x2 y2)
                               (- x2 y2))))))))

;;usage
(define (rec-perimeter r)
    (* (+ (rec-length r) 
          (rec-width r))
       2))

(define (rec-area r)
    (* (rec-length r)
       (rec-width r)))

