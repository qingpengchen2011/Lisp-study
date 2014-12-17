(load "eval.scm")
;;exercise-4.1-255

(define (list-of-value-left-to-right exps env)
  (if (no-operands exps)
      '()
      (let ((head (meval (first-operand exps) env)))
	(cons head (list-of-values-left-to-right (rest-operands exps) env)))))

(define (list-of-value-right-to-left exps env)
  (if (no-opereands exps)
      '()
      (let ((rest (list-of-values-right-to-left (rest-operands exps) env)))
	(cons (meval (first-operand exps) env) rest))))


;;exercise-4.2-259
;;b
(define (application? exp)
  (tagged-list? exp 'call))

(define (operator exp)
  (cadr exp))

(define (operands exp)
  (cddr exp))

;;exercise-4.4-259
;AND
(define (and? exp)
  (tagged-list? exp 'and))

(define (and-contents exps)
  (cdr exp))

(define (and-last? exp)
  (null? (cdr exp)))

(define (and-contents-empty? exp)
  (null? exp))

(define (and-content-first exps)
  (car exps))

(define (and-contents-rest exps)
  (cdr exps))

(define (eval-and exps env)
  (cond ((and-contents-empty? exps) 'true)
	((and-last? exps) (meval (and-content-first exps) env))
	(else
	 (if (true? (meval (and-content-first exps) env))
	     (eval-and (and-contents-rest exps))
	     'false))))
;OR
(define (or? exp)
  (tagged-list? exp 'or))

(define (or-contetns exp)
  (cdr exp))

(define (or-last? exp)
  (null? (cdr exp)))

(define (or-content-first exp)
  (car exp))

(define (or-contents-rest exp)
  (cdr exp))

(define (or-contents-empty? exp)
  (null? exp))

(define (eval-or exp env)
  (cond ((or-contents-empty? exp) 'false)
	((or-last? exp) (eval (or-content-first exp) env))
	((true? (meal (or-content-first exp) env)) 'true)
	(else
	 (eval-or (or-contents-rest exp) env))))

;and another way
(define (and->if exp)
  (cond ((and-contents-empty? exp) 'true)
	((and-last? exp) (make-if (and-content-first exp)
					  (and-content-first exp)
					  'false))
	(else
	 (make-if (and-content-first exp)
		  (and->if (and-contents-rest exp))
		  'false))))

;or another way
(define (or->if exp)
  (cond ((or-contents-empty? exp) 'false)
	((or-last? exp) (make-if (or-content-first exp) (or-content-first exp) 'false))
	(else
	 (make-if (or-content-first exp) (or-content-first exp) (or->if (or-contents-rest exp))))))


;;exercise-4.5-259

(define (cond-test_recipient-clause? clause)
  (eq? (cadr clause) '=>))

(define (cond-procedure clause)
  (caddr clause))

(define (cond-make-application procedure args)
  (list procedure args))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
	    (rest (cdr clauses)))
	(cond ((cond-else-clause? first)
	       (if (null? rest)
		   (sequence->exp (cond-actions first))
		   (error "ELSE clause isn't last -- COND->IF" clauses)))
	      ((cond-test_recipient-clause? first)
	       (make-if (cond-predicate first)
			(cond-make-application (cond-procedure first)
      					 (cond-predicate first))
			(expand-clauses rest)))
	      (else
	       (make-if (cond-predicate first)
			(sequence->exp (cond-actions first))
			(expand-clauses rest)))))))
;(cond->if '(cond ((assoc b ((a 1) (b 2))) => cadr) (else false)))
			
;;exercise-4.6-259
(define (let? exp)
  (tagged-list? exp 'let))

(define (let-body exp)
  (cddr exp))
  
(define (let-vars exp)
  (map car (cadr exp)))
  
(define (let-varexps exp)
  (map cadr (cadr exp)))
  
(define (let->combination exp)
  (cons (make-lambda (let-vars exp)
                     (let-body exp))
        (let-varexps exp)))

;;exercise-4.7-260
(define (let*? exp)
  (tagged-list? exp 'let*?))

(define (let*-body exp)
  (cddr exp))

(define (let*-bindings exp)
  (cadr exp))

(define (make-let binding body)
  (list 'let binding body))

(define (to-nested-lets bindings body)
  (if (null? bindings)
      (sequence->exp body)
      (make-let (list (car bindings))
                (to-nested-lets (cdr bindings)
                                body))))

(define (let*->nested-lets exp)
  (to-nested-lets (let*-bindings exp)
                  (let*-body exp)))
  
;;exercise-4.8-260
(define (named-let-funname exp)
  (cadr exp))
(define (named-let-vars exp)
  (map car (caddr exp)))
(define (named-let-varexps exp)
  (map cadr (caddr exp)))
(define (named-let-body exp)
  (cdddr exp))
(define (expand-body exp key)
  (cond ((null? exp) '())
        ((pair? (car exp))
         (cons (expand-body (car exp) key)
               (expand-body (cdr exp) key)))
        (else
         (cond ((eq? key (car exp))
                (append (cons (car exp)
                        (expand-body (cdr exp) key))
                        (list key)))
               (else
                (cons (car exp)
                      (expand-body (cdr exp) key)))))))
(define (named-let->let exp)
  (let* ((fun-lambda (make-lambda (append (named-let-vars exp) (list (named-let-funname exp)))
                          (expand-body (named-let-body exp) (named-let-funname exp))))
         (bindings (list (list (named-let-funname exp) fun-lambda)))
         (body (cons (named-let-funname exp)
                     (append (named-let-varexps exp)
                             (list (named-let-funname exp))))))
        (make-let bindings body)))

(define (named-let? exp)
  (symbol? (named-let-funname exp)))

(define (let->combination2 exp)
  (if (named-let? exp)
      (let->combination (named-let->let exp))
      (let->combination exp)))