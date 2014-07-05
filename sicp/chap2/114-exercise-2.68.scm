(load "114-exercise-2.67.scm")
;; encode
(define (encode1 message tree)
    (define (encode-symbol symbol current-branch)
        (define (next-branch-and-code symbol current-branch)
                    (if (leaf? current-branch)
                        (list '() '())
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
                  (display next) (newline)
                  (cons (car next)
                        (encode-symbol symbol (cadr next))))))
    (if (null? message)
        '()
        (append (encode-symbol (car message) tree)
                (encode1 (cdr message) tree))))

