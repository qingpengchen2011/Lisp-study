(load "constraints-system.scm")

(define (cel-fah-converter c f)
    (let ((u (make-connector))
	  (v (make-connector))
	  (w (make-connector))
	  (x (make-connector))
	  (y (make-connector)))
   	(multiplier c w u)
	(multiplier v x u)
	(adder v y f)
	(constant 9 w)
	(constant 5 x)
	(constant 32 y)
	'ok))


(define C (make-connector))
(define F (make-connector))
(cel-fah-converter C F)

(probe "Cel:" C)
(probe "Fah:" F)

