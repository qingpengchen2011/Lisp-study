(define (monte-carlo trials experiment)
    (define (iter trials-remains trials-pass)
        (cond ((= trials-remains 0) (/ trials-pass trials))
              ((experiment) (iter (- trials-remains 1) (+ trials-pass 1)))
	      (else (iter (- trials-remains 1) trials-pass))))
    (iter trials 0))

(define (monte-carlo-2 trials experiment)
    (define (iter-rec trials experiment)
        (cond ((= trials 0) 0)
              ((experiment) (+ (iter-rec (- trials 1) experiment) 1))
              (else (iter-rec (- trials 1) experiment))))
    (/ (iter-rec trials experiment) trials))


            
