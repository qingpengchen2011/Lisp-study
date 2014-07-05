(define (sum n)
  (if (= n 0)
      0
      (+ n (sum (- n 1)))))

(define (pasika line pos)
  (if (or (= pos (sum line))
          (= pos (+ 1
                    (sum (- line 1)))))
      1
      (+ (pasika (- line 1)
                 (- pos line))
         (pasika (- line 1)
                 (+ 1 (- pos line))))))          

(define (mypasika line index)
   (pasika line (+ index
                   (sum (- line 1)))))
