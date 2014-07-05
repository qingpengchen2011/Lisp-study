(define (double a)
   (* a 2))

(define (halve a)
   (/ a 2))

;;recursive
(define (fast-multi a b)
    (cond ((= b 0) 0)
          ((even? b) (double (fast-multi a (halve b))))
          (else (+ a (fast-multi a (+ b -1))))))

;;iterator
;;invariant s+ab
(define (fast-multi1 a b)
   (define (fast-multi-iter a b s)
      (cond ((= b 0) s)
            ((even? b) (fast-multi-iter (double a) (halve b) s)) 
            (else (fast-multi-iter a (+ b -1) (+ s a)))))
   (fast-multi-iter a b 0))
            


