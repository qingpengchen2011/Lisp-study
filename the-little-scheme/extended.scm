3;;define for atom?
(define (atom? x)
    (not (list? x)))

;;define for lat? 
;;where lat is a list of atoms
(define (lat? l)
    (cond ((null? l) '#t)
	  ((atom? (car l)) (lat? (cdr l)))
	  (else '#f)))

 ;;define for member
(define (member? m l)
    (cond ((null? l) '#f)
	  (else
	      (or (eq? m (car l))
		  (member? m (cdr l))))))

;;define for rember
(define (rember m lat)
    (cond ((null? lat) '())
	  ((eq? m (car lat)) (cdr lat))
	  (else
	     (cons (car lat) (rember m (cdr lat))))))

;;define for firsts
(define (firsts l)
    (cond ((null? l) '())
	  (else
	     (cons (car (car l))
		   (firsts (cdr l))))))

;;define for insertR
(define (insertR new old lat)
    (cond ((null? lat) '())
	  ((eq? old (car lat))
	   (cons (car lat)
		 (cons new (cdr lat))))
	  (else
	     (cons (car lat)
		   (insertR new old (cdr lat))))))

;;define for insertL
(define (insertL new old lat)
    (cond ((null? lat) '())
	  ((eq? old (car lat))
	   (cons new lat))
	  (else
	     (cons (car lat)
		   (insertL new old (cdr lat))))))

;;define for subst
(define (subst new old lat)
    (cond ((null? lat) '())
	  ((eq? old (car lat))
	   (cons new (cdr lat)))
	  (else
	     (cons (car lat)
		   (subst new old (cdr lat))))))

;;define for subst2
(define (subst2 new o1 o2 lat)
    (cond ((null? lat) '())
	  ((or (eq? o1 (car lat))
	       (eq? o2 (car lat)))
	   (cons new (cdr lat)))
	  (else
	     (cons (car lat)
		   (subst2 new o1 o2 (cdr lat))))))

;;define for multirember
(define (multirember m lat)
    (cond ((null? lat) '())
	  ((eq? m (car lat))
	   (multirember m (cdr lat)))
	  (else
	     (cons (car lat) 
		   (multirember m (cdr lat))))))

;;define for multiinsertr
(define (multiinsertr new old lat)
    (cond ((null? lat) '())
	  ((eq? old (car lat))
	   (cons old
		 (cons new
		       (multiinsertr new old (cdr lat)))))
	  (else
	     (cons (car lat)
		   (multiinsertr new old (cdr lat))))))

;;define for multiinsertl
(define (multiinsertl new old lat)
    (cond ((null? lat) '())
	  ((eq? old (car lat))
	   (cons new
		 (cons old (multiinsertl new old (cdr lat)))))
	  (else
	     (cons (car lat)
		   (multiinsertl new old (cdr lat))))))

;;define for multisubst
(define (multisubst new old lat)
    (cond ((null? lat) '())
	  ((eq? old (car lat))
	   (cons new (multisubst new old (cdr lat))))
	  (else
	     (cons (car lat)
		   (multisubst new old (cdr lat))))))

;;
;;chapter 4
;;

;;add1
(define (add1 n)
    (+ n 1))

;;sub1
(define (sub1 n)
    (- n 1))

;;add
(define (add n m)
    (cond ((zero? m) n)
	  (else 
	     (add1 (add n (sub1 m))))))

;;sub 
(define (sub n m)
    (cond ((zero? m) n)
	  (else
	     (sub1 (sub n (sub1 m))))))

;;addtup
(define (addtup tup)
    (cond ((null? tup) 0)
	  (else
	     (add (car tup) (addtup (cdr tup))))))

;;multiply
(define (multiply n m)
    (cond ((zero? m) 0)
	  (else
	     (add n (multiply n (sub1 m))))))

;;tup+
(define (tup+ tup1 tup2)
    (cond ((null? tup1) tup2)
	  ((null? tup2) tup1)
	  (else
	     (cons (add (car tup1) (car tup2))
		   (tup+ (cdr tup1) (cdr tup2))))))

;;great
(define (great n m)
    (cond ((zero? n) '#f)
	  ((zero? m) '#t)
	  (else
	     (great (sub1 n) (sub1 m)))))

;;less
(define (less n m)
    (cond ((zero? m) '#f)
	  ((zero? n) '#t)
	  (else
	     (less (sub1 n) (sub1 m)))))

;;equal?
(define (equal? n m)
    (cond ((less n m) '#f)
	  ((great n m) '#f)
	  (else
	     '#t)))

;;expt
(define (expt n m)
    (cond ((zero? m) 1)
	  (else
	    (multiply n (expt n (sub1 m))))))

;;divide
(define (divide n m)
    (cond ((less n m) 0)
	  (else
	     (add1 (divide (sub n m) m)))))

;;len
(define (len lat)
    (cond ((null? lat) 0)
	  (else 
	    (add1 (len (cdr lat))))))

;;pick
(define (pick n lat)
    (cond ((zero? (sub1 n)) (car lat))
	  (else
	     (pick (sub1 n) (cdr lat)))))

;;rempick
(define (rempick n lat)
    (cond ((zero? (sub1 n)) (cdr lat))
	  (else
	       (cons (car lat)
		     (rempick (sub1 n) (cdr lat))))))

;;no-nums
(define (non-nums lat)
    (cond ((null? lat) '())
	  ((number? (car lat)) (non-nums (cdr lat)))
	  (else
	     (cons (car lat) (non-nums (cdr lat))))))

;;all-nums
(define (all-nums lat)
    (cond ((null? lat) '())
	  ((number? (car lat))
		    (cons (car lat) (all-nums (cdr lat))))
	  (else
	     (all-nums (cdr lat)))))
;;eqatom?
(define (eqatom? a1 a2)
    (cond ((and (number? a1) (number? a2)) (equal? a1 a2))
	  ((or (number? a1) (number? a2)) '#f)
	  (else
	     (eq? a1 a2))))
;;occur
(define (occur a lat)
    (cond ((null? lat) 0)
	  ((eqatom? a (car lat)) (add1 (occur a (cdr lat))))
	  (else
	     (occur a (cdr lat)))))

;;
;;chapter 5
;;

;;rember*
(define (rember* a l)
    (cond ((null? l) '())
	  ((list? (car l)) (cons (rember* a (car l)) (rember* a (cdr l))))
	  ((eq? a (car l)) (rember* a (cdr l)))
	  (else
	     (cons (car l) (rember* a (cdr l))))))

;;insertR*
(define (insertR* new old l)
    (cond ((null? l) '())
	  ((atom? (car l))
	   (cond ((eq? old (car l)) (cons (car l)
					  (cons new
						(cdr l))))
		 (else
		    (cons (car l)
			  (insertR* new old (cdr l))))))
	  (else
	     (cons (insertR* new old (car l))
		   (insertR* new old (cdr l))))))

;;occur*
(define (occur* a l)
    (cond ((null? l) 0)
	  ((atom? (car l))
	   (cond ((eq? a (car l)) (1+ (occur* a (cdr l))))
		 (else
		    (occur* a (cdr l)))))
	  (else
	    (add (occur* a (car l))
		 (occur* a (cdr l))))))

;;subst*
(define (subst* new old l)
    (cond ((null? l) '())
	  ((atom? (car l))
	   (cond ((eq? old (car l)) (cons new (subst* new old (cdr l))))
		 (else
		    (cons (car l) (subst* new old (cdr l))))))
	  (else
	     (cons (subst* new old (car l))
		   (subst* new old (cdr l))))))

;;insertL*
(define (insertL* new old l)
    (cond ((null? l) '())
	  ((atom? (car l)) 
	   (cond ((eq? old (car l)) (cons new
					  (cons (car l)
						(insertL* new old (cdr l)))))
		 (else
		    (cons (car l) (insertL* new old (cdr l))))))
	  (else
	     (cons (insertL* new old (car l))
		   (insertL* new old (cdr l))))))

;;member*
(define (member* a l)
    (cond ((null? l) '#f)
	  ((atom? (car l)) (or (eq? a (car l)) (member* a (cdr l))))
	  (else
	     (or (member* a (car l)) (meber* a (cdr l))))))

;;eqlist?
(define (eqlist? l1 l2)
    (cond ((and (null? l1) (null? l2)) '#t)
	  ((or (null? l1) (null? l2)) '#f)
	  ((and (atom? (car l1)) (atom? (car l2)))
	   (and (eqatom? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
	  ((or (atom? (car l1)) (atom? (car l2))) '#f)
	  (else
	     (and (eqlist? (car l1) (car l2))
		  (eqlist? (cdr l1) (cdr l2))))))

;;eqsexpress? consponses to equal?
(define (eqsexpress? s1 s2)
    (cond ((and (atom? s1) (atom? s2))
	   (eqatom? s1 s2))
	  ((or (atom? s1) (atom? s2)) '#f)
	  (else
	     (eqlist? s1 s2))))
;;define eqsexpress2? with eqlist2?
(define (eqsexpress2? s1 s2)
    (cond ((and (atom? s1) (atom? s2)) (eqatom? s1 s2))
	  ((or (atom? s1) (atom? s2)) '#f)
	  (else
	     (eqlist2? s1 s2))))


;;define eqlist2? with eqsexpresss?
(define (eqlist2? l1 l2)
    (cond ((and (null? l1) (null? l2)) '#t)
	  ((or (null? l1) (null? l2)) '#f)
	  (else
	     (and (eqsexpress2? (car l1) (car l2))
		  (eqlist2? (cdr l1) (cdr l2))))))
;;define rember2 which remove the first matching s-expression in a list of s-expression in l
(define (rember2 s l)
    (cond ((null? l) '())
	  ((eqsexpress? s (car l)) (cdr l))
	  (else
	     (cons (car l) (rember2 s (cdr l))))))

;;
;;chapter 6
;;

;;numbered?
(define (numbered? aexp)
    (cond ((null? aexp) '#t)
	  ((atom? (car aexp))
	   (cond ((number? (car aexp)) (numbered? (cdr aexp)))
		((or (eq? 'add (car aexp))
		     (eq? 'multiply (car aexp))
		     (eq? 'exp (car aexp)))
		 (numbered? (cdr aexp)))
		(else
		 '#f)))
	  (else
	     (and (numbered? (car aexp)) (numbered? (cdr aexp))))))


;;
;;chapter 7
;;

;;makeset
(define (makeset1 lat)
    (cond ((null? lat) '())
	  ((member? (car lat) (cdr lat)) (makeset1 (cdr lat)))
	  (else
	     (cons (car lat)
		   (makeset1 (cdr lat))))))

(define (makeset2 lat)
    (cond ((null? lat) '())
	  (else
	     (cons (car lat)
		   (makeset2 (multirember (car lat) (cdr lat)))))))
;;set?
(define (set? lat)
    (or (null? lat)
	(and (not (member? (car lat) (cdr lat)))
	     (set? (cdr lat)))))

;;subset? is set1 is a subset of set2

(define (subset? set1 set2)
    (cond ((null? set1) '#t)
	  (else
	     (and (member? (car set1) set2)
		  (subset? (cdr set1) set2)))))

;;eqset?
(define (eqset1? set1 set2)
    (cond ((and (null? set1) (null? set2)) '#t)
	  ((or (null? set1) (null? set2)) '#f)
	  (else
	     (and (member? (car set1) set2)
		  (eqset1? (rember (car set1) set1)
			  (rember (car set1) set2))))))

(define (eqset2? set1 set2)
    (and (subset? set1 set2)
	 (subset? set2 set1)))

;;intersect?
(define (intersect? set1 set2)
    (cond ((null? set1) '#f)
	  (else
	     (or (member? (car set1) set2)
		 (intersect? (cdr set1) set2)))))

;;intersect
(define (intersect set1 set2)
    (cond ((null? set1) '())
	  ((member? (car set1) set2)
	   (cons (car set1) (intersect (cdr set1) set2)))
	  (else
	     (intersect (cdr set1) set2))))
;;union
(define (union set1 set2)
    (cond ((null? set1) set2)
	  ((member? (car set1) set2)
	   (union (cdr set1) set2))
	  (else
	     (cons (car set1) (union (cdr set1) set2)))))

;;intersect-all
(define (intersect-all all-set)
    (cond ((null? (cdr all-set)) (car all-set))
	  (else
	     (intersect (car all-set)
			(intersect-all (cdr all-set))))))

;;pair 
(define (first p)
    (car p))
(define (second p)
    (car (cdr p)))
(define (build-pair f s)
    (cons f (cons s '())))

;;a-pair?
(define (a-pair? pair)
     (cond ((atom? pair) '#f)
	   ((null? pair) '#f)
	   ((null? (cdr pair)) '#f)
	   ((null? (cdr (cdr pair))) '#t)
	   (else '#f)))

;;fun? 函数映射 
(define (fun? rel)
    (set? (firsts rel)))

;;revrel 
(define (revrel rel)
    (cond ((null? rel) '())
	  (else
	     (cons (build-pair (second (car rel))
			       (first (car rel)))
		   (revrel (cdr rel))))))

;;fullfun?
(define (fullfun? rel)
    (set? (firsts (revrel rel))))

;;
;;chapter 8
;;

;;multirember*co
(define (multirember*co a lat col)
    (cond ((null? lat)
	   (col '() '()))
	  ((eq? (car lat) a)
	   (multirember*co a
			   (cdr lat)
			   (lambda (newlat seen)
			     (col newlat
				  (cons (car lat) seen)))))
	  (else
	     (multirember*co a
			     (cdr lat)
			     (lambda (newlat seen)
			       (col (cons (car lat) newlat)
				    seen))))))

(multirember*co 'tune '(tune ha lo hu tune) build-pair)
;;
;;
;;build functions to collect more than one value at a time.
;;
;;  the trick is : use function args to collect values
;;
;;


;;multiinsertLR
(define (multiinsertLR new oldL oldR lat)
    (cond ((null? lat) '())
	  ((eq? (car lat) oldL)
	   (cons new (cons (car lat)
			    (multiinsertLR new oldL oldR (cdr lat)))))
	  ((eq? (car lat) oldR)
	   (cons (car lat) (cons new
				 (multiinsertLR new oldL oldR (cdr lat)))))
	  (else
	     (cons (car lat)
		   (multiinsertLR new oldL oldR (cdr lat))))))

;;multiinsertLR*co
(define (multiinsertLR*co new oldL oldR lat col)
    (cond ((null? lat) (col '() 0 0))
	  ((eq? (car lat) oldL)
	   (multiinsertLR*co new
			     oldL
			     oldR
			     (cdr lat)
			     (lambda (newlat L R)
			       (col (cons new
					  (cons (car lat)
						newlat))
				    (1+ L)
				    R))))
	  ((eq? (car lat) oldR)
		(multiinsertLR*co new 
				  oldL
				  oldR
				  (cdr lat)
				  (lambda (newlat L R)
				    (col (cons (car lat)
					       (cons new
						     newlat))
					 L
					 (1+ R)))))
	  (else
	     (multiinsertLR*co new
			       oldL
			       oldR
			       (cdr lat)
			       (lambda (newlat L R)
				 (col (cons (car lat)
					    newlat)
					    L
					    R))))))

;;even-only*
(define (even-only* l)
    (cond ((null? l) '())
	  ((atom? (car l))
	   (cond ((even? (car l))
		  (cons (car l) (even-only* (cdr l))))
		 (else
		    (even-only* (cdr l)))))
	  (else
	   (cons (even-only* (car l))
		 (even-only* (cdr l))))))

;;even-only*co
(define (even-only*co l col)
    (cond ((null? l)
	   (col '() 0 1))
	  ((atom? (car l))
	   (cond ((even? (car l))
		  (even-only*co (cdr l)
				(lambda (newl sum product)
				  (col (cons (car l)
					     newl)
				       sum
				       (* (car l) product)))))
		 (else
		    (even-only*co (cdr l)
				  (lambda (newl sum product)
				    (col newl
					 (+ sum (car l))
					 product))))))
	  (else
	     (even-only*co (car l)
			   (lambda (newl-car sum-car product-car)
			     (even-only*co (cdr l)
					   (lambda (newl-cdr sum-cdr product-cdr)
					     (col (cons newl-car newl-cdr)
						  (+ sum-car sum-cdr)
						  (* product-car product-cdr)))))))))

(define (even-only*co2 l col)
    (cond ((null? l)
	   (col '() 0 0))
	  ((atom? (car l))
	   (cond ((even? (car l))
		  (even-only*co2 (cdr l)
				 (lambda (newl sum product)
				   (col (cons (car l)
					      newl)
					sum
					(cond ((zero? product) (car l))
					      (else (* (car l) product)))))))
		 (else
		    (even-only*co2 (cdr l)
				   (lambda (newl sum product)
				     (col newl
					  (+ sum (car l))
					  product))))))
	  (else
	     (even-only*co2 (car l)
			    (lambda (newl-car sum-car product-car)
			      (even-only*co2 (cdr l)
					     (lambda (newl-cdr sum-cdr product-cdr)
					       (col (cons newl-car newl-cdr)
						    (+ sum-car sum-cdr)
						    (* (if (zero? product-car)
							   1
							   product-car)
						       (if (zero? product-cdr)
							   1
							   product-cdr))))))))))

;;
;;chapter 9
;;

(define (looking a lat)
    (keep-looking a (pick 1 lat) lat))
(define (pick n lat)
    (cond ((= n 1) (car lat))
	  (else
	     (pick (- n 1) (cdr lat)))))
(define (keep-looking a nors lat)
    (cond ((number? nors) (keep-looking a (pick nors lat) lat))
	  (else
	     (eq? a nors))))


;;align
;;shift:The function shift takes a pair whose first
;;      component is a pair and builds a pair by shifting the second
;;      part of the first component into the second component.
(define (shift pair)
    (build-pair (first (first pair))
		(build-pair (second (first pair))
			    (second pair))))

(define (align pora)
    (cond ((atom? pora) pora)
	  ((a-pair? (first pora))
	   (align (shift pora)))
	  (else
	   (build-pair (first pora)
		       (align (second pora))))))
(define A
  (lambda (n m)
    (cond ((zero? n) (add1 m))
	  ((zero? m) (A (sub1 n) 1))
	  (else
	   (A (sub1 n)
	      (A n (sub1 m)))))))

;;
;;chapter 10
;;

;;create a new entry
(define new-entry build-pair)

;;lookup-in-entry
(define lookup-in-entry
    (lambda (name entry entry-f)
      (lookup-in-entry-help name
			    (first entry)
			    (second entry)
			    entry-f)))
(define lookup-in-entry-help
    (lambda (name names values entry-f)
      (cond
         ((null? names) (entry-f name))
	 ((eq? (car names) name) (car values))
	 (else
	    (lookup-in-entry-help name
				  (cdr names)
				  (cdr values)
				  entry-f)))))

;;a table is the same as environment
;;Define the function extend-table which takes an entry
;; and a table (possibly the empty one) and creates a new table by
;; putting the new entry in front of the old table.

(define extend-table cons)

;;lookup-in-table
(define (lookup-in-table name table table-f)
    (cond ((null? table) (table-f name))
	  (else
	   (lookup-in-entry name
			     (car table)
			     (lambda (name)
			       (lookup-in-table name
						(cdr table)
						table-f))))))


