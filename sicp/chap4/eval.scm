
;;meval
(define (meval exp env)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-variable-value exp))
	((quoted? exp) (text-of-quotation exp))
	((assignment? exp) (eval-assignment exp env))
	((definition? exp) (eval-definition exp env))
	((if? exp) (eval-if exp env))
	((and? exp) (eval-and (and-contents exps) env))
	((or? exp) (eval-or (or-contents exps) env))
        ((let? exp) (meval (let->combination exp) env))
	((lambda? exp) (make-procedure (lambda-parameters exp)
				       (lambda-body exp)
				       env))
	((begin? exp) (eval-sequence (begin-action exp) env))
	((cond? exp) (eval (cond->if exp) env))
	((application? exp) (mapply (eval (operator exp) env)
				   (list-of-values (operands exp) env)))
	(else
	 (error "Unknown expression type--MEVAL" exp))))

;;helper funs

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (meval (first-operand exps) env)
	    (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (meval (if-predicate exp) env))
      (meval (if-consequent exp) env)
      (meval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (meval (first-exp exps) env))
	(else
	 (meval (first-exp exps) env)
	 (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
		       (meval (assignment-value exp) env)
		       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (meval (definition-value exp) env)
		    env)
  'ok)


;;representation of expressions
(define (self-evaluating? exp)
  (or (number? exp) (string? exp) '#f))

(define (variable? exp)
  (symbol? exp))


(define (tagged-list? exp tag)
  (or (and (pair? exp)
	   (eq? (car exp) tag))
      '#f))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp)
  (cadr exp))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp)
  (cadr exp))

(define (assignment-value exp)
  (caddr exp))

;;handle function definition syntactic sugar
(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)      ;parameters
		   (cddr exp))))    ;body

(define (lambda? exp)
  (tagged-list? exp 'lambda))

(define (lambda-paramters exp)
  (cadr exp))

(define (lambda-body exp)
  (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp)
  (tagged-list? exp 'if))

(define (if-predicte exp)
  (cadr exp))

(define (if-consequent exp)
  (caddr exp))

(define (if-alternative exp)
  (if (null? (cdddr exp))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp)
  (tagged-list? exp 'begin))

(define (begin-acions exp)
  (cdr exp))

(define (first-exp seq)
  (car seq))

(define (rest-exps seq)
  (cdr seq))

(define (last-exp? seq)
  (null? (cdr seq)))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
	((last-exp? seq) (first-exp seq))
	(else (make-begin seq))))

(define (make-begin seq)
  (cons 'begin seq))

(define (application? exp)
  (pair? exp))

(define (operator exp)
  (car exp))

(define (operands exp)
  (cdr exp))

(define (no-operand? ops)
  (null? ops))

(define (first-operand ops)
  (car ops))

(define (rest-operands ops)
  (cdr ops))

(define (cond? exp)
  (tagged-list? exp 'cond))

(define (cond-clauses exp)
  (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause)
  (car clause))

(define (cond-actions clause)
  (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
	    (rest (cdr clauses)))
	(cond ((cond-else-clause? first)
	       (if (null? rest)
		   (sequence->exp (cond-actions first))
		   (error "ELSE clause is not in the last --COND->IF" clauses)))
	      (else
	       (make-if (cond-predicate first)
			(cond-actions first)
			(expand-clauses rest)))))))



;;mapply
(define (mapply procedure arguments)
  (cond ((primitive-procedure? procedure)
	 (apply-primitive-procedure procedure arguments))
	((compound-procedure? procedure)
	 (eval-sequence (procedure-body procedure)
			(extend-environment (procedure-parameters procedure)
					    arguments
					    (procedure-environment procedure))))
	(else
	 (error "Unknow procedure type--MAPPLY" procedure))))

(define (true? x)
  (not (eq? x 'false)))
(define (false? x)
  (eq? x 'false))

(define (make-procedure parameters body env)
  (list 'procedure parameters body  env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p)
  (cadr p))

(define (procedure-body p)
  (caddr p))

(define (procedure-environment p)
  (cadddr p))

;;environment
(define (enclosing-environment env)
  (cdr env))

(define (first-frame env)
  (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame)
  (car frame))

(define (frame-values frame)
  (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (frame-variables frame)))
  (set-cdr! frame (cons val (frame-values frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals)
            base-env)
      (if (< (length vars) (length vals))
          (error "too many arguments supplied" vars vals)
          (error "too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else
             (scan (cdr vars) (car vals)))))
    (cond ((eq? env the-empty-environment)
           (error "Unbound variable" var))
          (else
           (scan (frame-variables (first-frame env))
                 (frame-values (first-frame env))))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else
             (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "unbound variable --SET!" var val)
        (let ((frame (first-frame env)))
          (scan (frame-variables env)
                (frame-values env)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq var (car vars))
             (set-car! vals val))
            (else
             (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables env) (frame-values env))))


