(define (make-queue)
    (let ((front-ptr '())
	  (rear-ptr '()))
         (define (get-front-ptr) front-ptr)
         (define (get-rear-ptr) rear-ptr)
         (define (set-front-ptr! item) (set! front-ptr item))
	 (define (set-rear-ptr! item) (set! rear-ptr item))
         (define (empty-queue?) (null? (get-rear-ptr)))
         (define (front-queue)
		   (if (empty-queue?)
		       (error "FRONT called with an empty queue!" (cons front-ptr rear-ptr))
		       (car (get-front-ptr))))
         (define (insert-queue! item)
		(let ((new-pair (cons item '())))
                   (if (empty-queue?)
                       (begin (set-front-ptr! new-pair)
			      (set-rear-ptr! new-pair))
		       (begin (set-cdr! (get-rear-ptr) new-pair)
			      (set-rear-ptr! new-pair))))
		  front-ptr)
	 (define (delete-queue! )
		   (if (empty-queue?)
		       (error "DELETE called with an empty queue!" (cons front-ptr rear-ptr))
		       (set-front-ptr! (cdr (get-front-ptr))))
		   front-ptr)
         (define (print-queue)
                  (if (empty-queue?)
		      '()
	              (get-front-ptr)))
         ;;dispatch 
	 (define (dispatch m)
             (cond ((eq? m 'insert)  insert-queue!)
		   ((eq? m 'delete) delete-queue!)
          	   ((eq? m 'front) front-queue)
		   ((eq? m 'empty?) empty-queue?)
 		   ((eq? m 'print) print-queue)
		   (else
  			(error "Unknow-command" m))))
         dispatch))
          
