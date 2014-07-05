(load "flatmap.scm")
(load "enumerate-interval.scm")

(define (make-triple0 n)
  (accumulate append '()
    (flatmap (lambda (i)
                 (map (lambda (j)
                          (map (lambda (k)
                                   (list i j k))
                               (enumerate-interval 1 n)))
                      (enumerate-interval 1 n)))
             (enumerate-interval 1 n))))

(define (make-triple1 n)
    (flatmap (lambda (tuple)
                 (map (lambda (i)
                          (cons i tuple))
                 (enumerate-interval 1 n)))
             (flatmap (lambda (j)
                          (map (lambda (k)
                                   (list j k))
                          (enumerate-interval 1 n)))
                      (enumerate-interval 1 n))))

(define (alldiff? triple)
    (let ((i (car triple))
          (j (cadr triple))
          (k (caddr triple)))
         (and (not (= i j))
              (not (= i k))
              (not (= j k)))))

(define (unique-triple n)
    (filter alldiff?
	    (make-triple0 n)))

(define (triple-sum-equal-to? sum triple)
    (= sum
       (fold-right + 0 triple)))

(define (sun-=-s s n)
    (filter (lambda (triple)
                (= s
                   (+ (car triple)
                      (cadr triple)
                      (caddr triple))))
            (unique-triple n)))
