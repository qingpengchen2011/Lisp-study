(load "type-content.scm")
(define (apply-generic op . args)
    (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents args))
                (error "No Method for these types--APPLY-GENERIC" (list op type-tags))))))
