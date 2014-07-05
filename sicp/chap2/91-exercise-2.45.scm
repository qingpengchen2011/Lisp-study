(define (split0 big-combiner small-combiner)
    (lambda (painter n)
        (if (= n 0)
            painter
            (let ((small ((split big-combiner small-combiner) painter 
							      (- n 1))))
                 (big-combiner painter
			       (small-combiner small small))))))


(define (split1 big-combiner small-combiner)
    (define (inner painter n)
        (if (= n 0)
            painter
	    (let ((small (inner painter (- n 1))))
		 (big-combiner painter 
				(small-combiner small small))))))

(define right-split (split0 beside below))

(define up-split (split0 below beside))
