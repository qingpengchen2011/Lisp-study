(define (lookup key-1 key-2 table)
    (let ((sub-table (assoc key-1 (cdr table))))
	 (if sub-table
	     (let ((record (assoc key-2 (cdr sub-table))))
		  (if record
		      (cdr record)
		      '#f))
	     '#f)))

(define (insert! key-1 key-2 value table)
    (let ((sub-table (assoc key-1 (cdr table))))
	 (if sub-table
	     (let ((record (assoc key-2 (cdr sub-table))))
		  (if record
		      (set-cdr! record value)
		      (set-cdr! sub-table (cons (cons key-2 value)
					        (cdr sub-table)))))
         (set-cdr! table (cons (list key-1
			             (cons key-2 value))
			       (cdr table)))))
    'ok)

(define (make-2table)
    (list '*table*))
