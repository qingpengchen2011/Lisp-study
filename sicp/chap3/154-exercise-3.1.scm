(define (make-accumulator v)
    (lambda (add-v)
        (set! v (+ add-v v))
	     v))
	  	    
		    
