;;representation of leaf-node
(define (make-leaf symbol weight)
    (list 'leaf symbol weight))
(define (leaf? ob)
    (eq? 'leaf (car ob)))
(define (leaf-symbol leaf)
    (cadr leaf))
(define (leaf-weight leaf)
    (caddr leaf)) 
;;representation of tree node
(define (make-code-tree left right)
    (list left 
	  right 
	  (append (symbols left) (symbols right))
	  (+ (weight left) (weight right))))
(define (left-branch tree)
    (car tree))
(define (right-branch tree)
    (cadr tree))
(define (symbols tree)
    (if (leaf? tree)
        (list (leaf-symbol tree))
        (caddr tree)))			
(define (weight tree)
    (if (leaf? tree)
	(leaf-weight tree)
        (cadddr tree)))

;; decode
(define (decode bits tree)
    (define (choose-branch bit current-branch)
        (cond ((= bit 0)
		  (left-branch current-branch))
	      ((= bit 1)
		  (right-branch current-branch))
	      (else
		  (error "Bad Bit!"))))
    (define (inner-decode bits current-branch)
	(if (null? bits)
 	    '()
	    (let ((next-branch (choose-branch (car bits)
	        			      current-branch)))
             	 (if (leaf? next-branch)
		     (cons (leaf-symbol next-branch)
		           (inner-decode (cdr bits) tree))
		     (inner-decode (cdr bits) next-branch)))))
    (inner-decode bits tree))

;; encode
(define (encode message tree)
    (define (encode-symbol symbol current-branch)
    	(define (next-branch-and-code symbol current-branch)
		(let ((left (left-branch current-branch))
		      (right (right-branch current-branch)))
		     (cond ((element-of-set? symbol (symbols left))
                               (list 0 left))
 			   ((element-of-set? symbol (symbols right))
                               (list 1 right))
                           (else
 			       (error "unrecognize symbol" symbol)))))   ;an unrecognize symbol should be check at the root of tree
        (if (leaf? current-branch)
	    '()
	    (let ((next (next-branch-and-code symbol current-branch)))
                  (cons (car next)
			(encode-symbol symbol (cadr next))))))
    (if (null? message)
        '()
    	(append (encode-symbol (car message) tree)
		(encode (cdr message) tree))))
;;another version encode
;;!! needs to clean unaccepted '() in result
(define (encode1 message tree)
    (define (encode-symbol symbol current-branch)
        (define (next-branch-and-code symbol current-branch)
                    (if (leaf? current-branch)
                        (list '() '())  ;where unaccpted '() comes from
                        (let ((left (left-branch current-branch))
                              (right (right-branch current-branch)))
                              (cond ((element-of-set? symbol (symbols left))
                                          (list 0 left))
                                    ((element-of-set? symbol (symbols right))
                                          (list 1 right))
                                    (else
                                          (error "unrecognize symbol" symbol))))))
        (if (null? current-branch)
            '()
            (let ((next (next-branch-and-code symbol current-branch)))
                  (cons (car next)
                        (encode-symbol symbol (cadr next))))))
    (if (null? message)
        '()
        (append (encode-symbol (car message) tree)
                (encode1 (cdr message) tree))))

;;---------------------------------------------------------------


;;set of tree nodes
(define (adjoin-set x set)
    (cond ((null? set) (list x))
          ((< (weight x) (weight (car set)))
		(cons x set))
          (else
		(cons (car set) (adjoin-set x (cdr set))))))

(define (element-of-set? x set)
    (cond ((null? set) '#f)
	  ((eq? x (car set)) '#t)
          (else
		(element-of-set? x (cdr set)))))

;;make-init-leaf-set
(define (make-leaf-set pairs)
     (if (null? pairs)
         '()
         (adjoin-set (make-leaf (caar pairs)
				(cadar pairs))
 		     (make-leaf-set (cdr pairs)))))


	
	
    
        
