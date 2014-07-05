(define (myassoc key alist)
    (cond ((null? alist) '#f)
	  ((equal? key (caar alist)) (car alist))
	  (else
		(myassoc key (cdr alist)))))

(define (lookup key table)
    (let ((record (assoc key (cdr table))))
	 (if record
	     (cdr record)
	     '#f)))
(define (insert! key value table)
    (let ((record (assoc key (cdr table))))
	 (if record
	     (set-cdr! record value)
	     (begin (let ((new-record (cons key value)))
			 (let ((new-node (cons new-record (cdr table))))
			      (set-cdr! table new-node)))))))
(define (make-1table)
    (list '*table*))
