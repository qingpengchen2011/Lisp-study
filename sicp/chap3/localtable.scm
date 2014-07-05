(define (make-table)
    (let ((local-table (list '*table*)))
	 (define (assoc key alist)
             (cond ((null? alist) '#f)
		   ((equal? key (caar alist)) (car alist))
		   (else 
			(assoc key (cdr alist)))))
	 (define (lookup key-1 key-2)
             (let ((sub-table (assoc key-1 (cdr local-table))))
		  (if sub-table
		      (let ((record (assoc key-2 (cdr sub-table))))
			   (if record
			       (cdr record)
			       '#f))
		      '#f)))
	 (define (insert! key-1 key-2 value)
	     (let ((sub-table (assoc key-1 (cdr local-table))))
		  (if sub-table
		      (let ((record (assoc key-2 (cdr sub-table))))
			   (if record
			       (set-cdr! record value)
			       (set-cdr! sub-table (cons (cons key-2 value)
							 (cdr sub-table)))))
		      (set-cdr! local-table (cons (list key-1 
							(cons key-2 value))
						  (cdr local-table)))))
	 'ok)
        (define (dispatch m)
	    (cond ((eq? 'lookup m) lookup)
		  ((eq? 'insert! m) insert!)
		  (else
		      (error "unknow-command" m))))
    dispatch))
		
(define table-operation (make-table))
(define get (table-operation 'lookup))
(define put (table-operation 'insert!))
