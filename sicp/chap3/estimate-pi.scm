(load "monte-carlo.scm")

(define (cesaro-test)
     (= (gcd (random 1000) (random 2000)) 1))

(define (estimate-pi trials)
    (sqrt (/ 6 (monte-carlo trials cesaro-test))))
        
(define (estimate-pi-2 trials)
    (sqrt (/ 6 (monte-carlo-2 trials cesaro-test))))


