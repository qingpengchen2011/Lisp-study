
;;load something from the-little-schemer
(load "/Users/tonychen/Desktop/workspace/GitHub/Lisp-study/trunk/the-little-scheme/extended.scm")

;;
;;chapter11
;;


;;sum-of-prefix
(define sum-of-prefix (lambda (lat)
			(sum-of-prefix-b 0 lat)))
			   
(define sum-of-prefix-b (lambda (ssf lat)
			  (cond ((null? lat) '())
				(else
				 (cons (+ ssf (car lat))
				       (sum-of-prefix-b (+ ssf (car lat))
							(cdr lat)))))))

;;
;;eleventh commndement
;;
;;  use addtional arguments when recursion function needs to now something
;;  that happend before


;;scramble
(define scramble (lambda (tup)
		   (scramble-b tup '())))
(define scramble-b (lambda (tup rev-prex)
		     (cond ((null? tup) '())
			   (else
			    (cons (pick (car tup)
					(cons (car tup) rev-prex))
				  (scramble-b (cdr tup) (cons (car tup) rev-prex)))))))

;;
;;chapter 12
;;

;;multirember using letrc
;;name it as letrec-multirember
(define letrec-multirember (lambda (a lat)
			    (letrec ((mr (lambda (lat)
					  (cond ((null? lat) '())
						((eq? a (car lat))
						 (mr (cdr lat)))
						(else
						 (cons (car lat)
						       (mr (cdr lat))))))))
				   (mr lat))))
abortive
;;letrec-multirember-f
(define letrec-multirember-f (lambda (test?)
			       (lambda (a lat)
				 (letrec ((mr-f (lambda (lat)
						  (cond ((null? lat) '())
							((test? a (car lat))
							 (mr-f (cdr lat)))
							(else
							 (cons (car lat)
							       (mr-f (cdr lat))))))))
				   (mr-f lat)))))
;;letrec-rember?
(define letrec-rember? (lambda (a lat)
			 (letrec ((r (lambda (lat)
				       (cond ((null? lat) '#f)
					     (else
					      (or (eq? (car lat) a)
						  (r (cdr lat))))))))
			   (r lat))))

;;letrec-union
(define letrec-union (lambda (set1 set2)
		       (letrec ((u (lambda (set1)
				     (cond ((null? set1) set2)
					   ((member? (car set1) set2)
					    (u (cdr set1)))
					   (else
					    (cons (car set1)
						  (u (cdr set1))))))))
			 (u set1))))

;;letrec-union2
;;define the function within another function
;;to hide the function
(define letrec-union2 (lambda (set1 set2)
			(letrec ((m? (lambda (a lat)
				       (letrec ((n? (lambda (lat)
						      (cond ((null? lat) '#f)
							    (else
							     (or (eq? a (car lat))
								 (n? (cdr lat))))))))
					 (n? lat))))
				 (u (lambda (set1)
				      (cond ((null? set1) set2)
					    ((m? (car set1) set2)
					     (u (cdr set1)))
					    (else
					     (cons (car set1)
						   (u (cdr set1))))))))
			  (u set1))))
				 
				      
;;letrec-sumofprefix
(define letrec-sumofprefix (lambda (tup)
			     (letrec ((s (lambda (sss tup)
					(cond ((null? tup) '())
					      (else
					       (cons (+ sss (car tup))
						     (s (+ sss (car tup))
							(cdr tup))))))))
			       (s 0 tup))))
;;because the s within letrec-sumofprefix does not need
;;to know about the arguement tup
;;so we can rewrite it as:
(define letrec-sumofprefix2 (letrec
				((s (lambda (sss tup)
				      (cond ((null? tup) '())
					    (else
					     (cons (+ sss (car tup))
						   (s (+ sss (car tup))
						      (cdr tup))))))))
			      (lambda (tup)
				(s 0 tup))))


;;		   
;;chapter 13
;;
(define intersectall (lambda (lset)
		       (letrec ((A (lambda (lset)
				     (cond ((null? (cdr lset)) (car lset))
					   (else
					    (intersect (car lset)
						       (A (cdr lset))))))))
			 (cond ((null? lset) '())
			       (else
				(A lset))))))
			 
			 
;;
;;------
;;letcc
;;-----
;;

;(define letcc-intersectall (lambda (lset)
;			     (letcc hop
;				    (letrec ((A (lambda (lset)
;						  (cond ((null? (car lset))
;							 (hop '()))
;							((null? (cdr lset))
;							 (car lset))
;							(else
;							 (intersect (car lset)
;								    (A (cdr lset))))))))
;				      (cond ((null? lset) '())
;					    (else
;					     (A lset)))))))

;;-----------------------------
;;call-with-current-continuation
;;-----------------------------
(define scheme-intersectall (lambda (lset)
			      (call-with-current-continuation
			       (lambda (hop)
				 (letrec ((A (lambda (lset)
					       (cond ((null? (car lset))
						      (hop '()))
						     ((null? (cdr lset))
						      (car lset))
						     (else
						      (intersect (car lset)
								 (A (cdr lset))))))))
				   (cond ((null? lset) '())
					 (else
					  (A lset))))))))

;;a new version of scheme-intersectall
;; use hop inside function intersect
(define scheme-intersectall2 (lambda (lset)
			       (call-with-current-continuation (lambda (hop)
								 (letrec ((A (lambda (lset)
									       (cond ((null? (car lset))
										      (hop '()))
										     ((null? (cdr lset))
										      (car lset))
										     (else
										      (I (car lset)
											 (A (cdr lset)))))))
									  (I (lambda (set1 set2)
									       (letrec ((J (lambda (set1)
											    (cond ((null? set1) '())
												  ((member? (car set1) set2)
												   (cons (car set1)
													 (J (cdr set1))))
												  (else
												   (J (cdr set1)))))))
										 (cond ((null? set2) (hop '()))
										       (else
											(J set1)))))))
								   (cond ((null? lset) '())
									 (else
									  (A lset))))))))
											 
						      
						
;;define rember-upto-last
(define rember-upto-last (lambda (a lat)
			   (call-with-current-continuation (lambda (skip)
							     (letrec ((R (lambda (lat)
									   (cond ((null? lat) '())
										 ((eq? (car lat) a)
										  (skip (cons 'test (R (cdr lat)))))
										 (else
										  (cons (car lat)
											(R (cdr lat))))))))
							       (R lat))))))

;;
;;chapter 14
;;

(define let-leftmost (lambda (lat)
		       (cond ((null? lat) '())
			     ((atom? (car lat))
			      (car lat))
			     (else
			      (letrec ((r (let-leftmost (car lat))))
				(cond ((atom? r) r)
				      (else
				       (let-leftmost (cdr lat)))))))))
	       
;;rember*1
(define rember*1 (lambda (a lat)
  (letrec ((R (lambda (lat)
		(cond ((null? lat) '())
		      ((atom? (car lat))
		       (cond ((eq? a (car lat))
			      (cdr lat))
			     (else
			      (cons (car lat)
				    (R (cdr lat))))))
		      (else
		       (let ((carv (R (car lat))))
			 (cond ((eqlist? (car lat) carv)
				(cons (car lat)
				      (R (cdr lat))))
			       (else
				(cons carv (cdr lat))))))))))
    (R lat))))

;;define depth*
(define depth* (lambda (lat)
		 (cond ((null? lat) 1)
		       ((atom? (car lat))
			(depth* (cdr lat)))
		       (else
			(let ((carv (1+ (depth* (car lat))))
			      (cdrv (depth* (cdr lat))))
			  (cond ((> carv cdrv)
				 carv)
				(else
				 cdrv)))))))
;;leftmost2
;;leftmost with call-with-current-continuation
;;    to skip once we get the answer rather than
;;    return back level by level
(define leftmost2 (lambda (lat)
		    (letrec ((lm (lambda (lat skip)
				   (cond ((null? lat) '())
					 ((atom? (car lat))
					  (skip (car lat)))
					 (else
					  (begin (lm (car lat) skip)
						 (lm (cdr lat) skip)))))))
		      (call-with-current-continuation (lambda (skip)
							(lm lat skip))))))
		    
(define leftmost3 (lambda (lat)
		    (call-with-current-continuation (lambda (skip)
						      (letrec ((lm (lambda (lat)
								     (cond ((null? lat) '())
									   ((atom? (car lat))
									    (skip (car lat)))
									   (else
									    (begin (lm (car lat))
										   (lm (cdr lat))))))))
							(lm lat))))))
										  
							       
;;using letcc and let in rember*2
(define rember*2 (lambda (a lat)
		   (letrec ((R (lambda (lat skipup)
				 (cond ((null? lat)
					(skipup 'no))
				       ((atom? (car lat))
					(cond ((eq? (car lat) a)
					       (cdr lat))
					      (else
					       (cons (car lat)
						     (R (cdr lat) skipup)))))
				       (else
					(let ((r (call-with-current-continuation (lambda (skipup)
										   (R (car lat) skipup)))))
					  (cond ((atom? r)
						 (cons (car lat)
						       (R (cdr lat) skipup)))
						(else
						 (cons r (cdr lat))))))))))
		     (call-with-current-continuation (lambda (skipup)
						       (let ((r (R lat skipup)))
							 (cond ((atom? r) lat)
							       (else
								r))))))))
								   
		  
;;
;;chapter 15
;;



;;
;;chapter16
;;





;;
;;chapter 17
;;
;;letcc and memorize to reduce computations
;;



;;
;;chapter 18
;;


(define bons (lambda (kar)
	       (let ((kdr '()))
		 (lambda (selector)
		   (selector
		    (lambda (x) (set! kdr x))
		    kar
		    kdr)))))

(define set-kdr (lambda (c x)
		  ((c (lambda (s a d) s)) x)))

(define kar (lambda (c)
	      (c (lambda (s a d) a))))

(define kdr (lambda (c)
	      (c (lambda (s a d) d))))

(define kons (lambda (a d)
	       (let ((c (bons a)))
		 (set-kdr c d)
		 c)))

;;
;;chapter 19
;;

(define toppings)

(define deepB (lambda (n)
		(if (zero? n)
		    (call-with-current-continuation (lambda (jump)
						      (set! toppings jump)
						      'pizza))
		    (cons (deepB (- n 1))
			  '()))))

(define deepcoB (lambda (n k)
		  (if (zero? n)
		      (begin 
			(set! toppings k)
			(k 'pizza))
		      (deepcoB (- n 1)
			       (lambda (x)
				 (k (cons x '())))))))

;;diffrences between letcc and collector k
;;these two things are different


;;
;;new things to illustrate
;;
(define leave)
(define fill)

(define start-it (lambda (l)
		   (call-with-current-continuation (lambda (here)
						     (set! leave here)
						     (waddle l)))))
(define waddle (lambda (l)
		 (cond ((null? l) '())
		       ((atom? (car l))
			(begin (call-with-current-continuation (lambda (rest)
							  (set! fill rest)
							  (leave (car l))))
			       (waddle (cdr l))))
							  
		       (else
			(begin (waddle (car l))
			       (waddle (cdr l)))))))
							

;;
;;chapter 20
;;

(define the-empty-table (lambda (name)
			  ))

(define lookup (lambda (table name)
		 (table name)))

(define extend (lambda (name value table)
		 (lambda (name_lookedup)
		   (if (eq? name name_lookedup)
		       value
		       (table name_lookedup)))))