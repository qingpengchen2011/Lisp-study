(load "flatmap.scm")
(load "enumerate-interval.scm")
(load "square.scm")

(define empty-board '())

;;result as ((col-1 row) (col-2 row) ... (col-n rwo))
(define (adjoin-position new-row k rest-of-queens)
    (append rest-of-queens (list (list k new-row))))

(define (safe? k positions)
    (define (position-kth k positions)
        (if (= k 1)
            (car positions)
            (position-kth (- k 1) 
		          (cdr positions))))

    (define (validate? ipos kpos)
        (define (inline? ipos kpos)
            (= (square (- (car ipos)
                          (car kpos)))
               (square (- (cadr ipos)
                          (cadr kpos)))))
        (define (samerow? ipos kpos)
            (= (cadr ipos)
               (cadr kpos)))
        (and (not (inline? ipos kpos))
             (not (samerow? ipos kpos))))

    (define (checkwith-i i k)
        (if (= i 0)
            '#t
            (and (validate? (position-kth i positions)
                            (position-kth k positions))
                 (checkwith-i (- i 1) k))))

    (checkwith-i (- k 1) k))

    

(define (queens board-size)
    (define (queen-cols k)
        (if (= k 0)
            (list empty-board)
            (filter 
                (lambda (positions) (safe? k positions))
                (flatmap 
                    (lambda (rest-of-queens)
                        (map (lambda (new-row)
                                (adjoin-position new-row k rest-of-queens))
                             (enumerate-interval 1 board-size)))
                    (queen-cols (- k 1))))))
    (queen-cols board-size))

(define (queens-show board-size)
    (for-each (lambda (seq)
             (display seq)
             (newline))
         (queens board-size)))
