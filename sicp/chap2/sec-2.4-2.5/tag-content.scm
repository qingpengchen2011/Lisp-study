(define (attach-tag tag content)
    (if (number? content)
        content
        (cons tag content)))

(define (type-tag datum)
    (cond ((number? datum) 'scheme-number)
          ((pair? datum) (car datum))
	  (else
 	     (error "Bad tagged datum--TYPE-TAGE" datum))))

(define (contents datum)
    (cond ((number? datum) datum)
          ((pair? datum) (cdr datum))
          (else
	     (error "Bad tagged datum --CONTENTS " datum))))


