(define (or-gate x y s)
    (let ((a (make-wire))
	  (b (make-wire))
	  (c (make-wire)))
	 (inverter x a)
         (inverter y b)
         (and-gate a b c)
         (inverter c s)
         'ok))

;;or-gate-delay=2*inverter-delay + and-gate-delay
	 
    
