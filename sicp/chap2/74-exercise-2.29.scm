(define (make-mobile left right)
    (list left right))

(define (make-branch len structure)
    (list len structure))

(define (left-branch mobile)
   (car mobile))

(define (right-branch mobile)
   (cadr mobile))

(define (branch-length branch)
   (car branch))

(define (branch-structure branch)
   (cadr branch))

;;
(define (more-mobile? branch)
    (pair? (branch-structure branch)))

(define (total-weight mobile)
   (define (branch-weight branch)
        (let ((structure (branch-structure branch)))
             (if (not (pair? structure))
                 structure
                 (total-weight structure))))
   (+ (left-branch mobile)
      (right-branch mobile)))

(define (balance-mobile mobile)
    (define (branch-moment branch)
         (* (branch-length branch)
            (total-weight (branch-structure 
		            branch))))
    (define (branch-moment-eq? mobile)
        (= (branch-moment (left-branch mobile))
           (branch-moment (right-branch mobile))))

    (let ((left-structure (branch-structure (left-branch mobile)))
          (right-structure (branch-structure (right-branch mobile))))
         (cond ((
