(define (smallest-divisor n)
   (find-divisor n 2))

(define (find-divisor n test-divisor)
  (define (divides?)
     (= (remainder n test-divisor) 0))
  (cond ((> (square test-divisor) n) n)
        ((divides?) test-divisor)
        (else (find-divisor n (+ 1 test-divisor)))))

(define (prime? n)
   (= (smallest-divisor n) n))

;;timed-prime-test

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (real-time-clock)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-time (- (real-time-clock) start-time))
      #f))

(define (report-time time-elapes)
    (display "***")
    (display time-elapes)
    #t)

;;
(define (next-odd n)
   (if (= (remainder n 2) 0)
       (+ n 1)
       (+ n 2)))

(define (search-for-primes start count)
   (cond ((= count 0) (display "are primes!"))
         ((not (timed-prime-test start)) (search-for-primes (next-odd start) count))
         (else (search-for-primes (next-odd start) (- count 1)))))


