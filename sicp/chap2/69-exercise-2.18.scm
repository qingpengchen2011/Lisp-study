(define (myreverse0 l)
    (define (inner rl cl)
        (if (null? cl)
            rl
            (inner (cons (car cl)
                         rl)
                   (cdr cl))))
    (inner '() l))

(define (myreverse1 l)
    (if (null? l)
        '()
        (append (myreverse1 (cdr l)) (list (car l)))))
