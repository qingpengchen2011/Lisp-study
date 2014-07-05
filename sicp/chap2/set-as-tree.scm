(define (make-tree entry left right)
     (list entry left right))

(define (entry tree)
     (car tree))
(define (left-branch tree)
     (cadr tree))
(define (right-branch tree)
     (caddr tree))


(define (element-of-set? x set)
    (cond ((null? set) '#f)
          ((< x (entry set))
 		(element-of-set? x (left-branch set)))
	  ((= x (entry set)) '#t)
          (else 
		(element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
    (cond ((null? set)
		(make-tree x '() '()))
	  ((< x (entry set))
		(make-tree (entry set)
			   (adjoin-set x (left-branch set))
			   (right-branch set)))
	  (else
	      (make-tree (entry set)
			 (left-branch set)
			 (adjoin-set x (right-branch set))))))

(define (tree->list1 tree)
     (if (null? tree)
           '()
	 (append (tree->list1 (left-branch tree))
		 (cons (entry tree) 
		       (tree->list1 (right-branch tree))))))
		
(define (tree->list2 tree)
     (define (copy-to-list tree result)
          (if (null? tree)
              result
	      (copy-to-list (left-branch tree)
 			    (cons (entry tree)
				  (copy-to-list (right-branch tree)
					  	result)))))
     (copy-to-list tree '()))
