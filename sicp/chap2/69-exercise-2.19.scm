(define (no-more? l)
    (null? l))

(define (except-first-denomination l)
    (cdr l))

(define (first-denomination l)
    (car l))

(define (cc amount coin-values)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (no-more? coin-values)) 0)
          (else
              (+ (cc amount (except-first-denomination coin-values))
                 (cc (- amount (first-denomination coin-values))
                     coin-values)))))

(define uk-coins (list 100 50 20 5 10 2 1 10 0.5))

(define (test-uk)
    (let ((start (real-time-clock)))
         (display "Start:")
         (display start)
         (newline)
         (cc 140 uk-coins)
      (let ((end (real-time-clock)))
         (let ((b (- end start)))
              (display  "End:")
              (display end)
              (newline)
              (display "duration:")
              (display b)
              (newline)
              (internal-time/ticks->seconds b)))))
