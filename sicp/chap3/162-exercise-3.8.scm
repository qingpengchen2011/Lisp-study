(define f
    (let ((current-v 0))
         (lambda (n)
             (let ((rt current-v))
                  (set! current-v n)
                  rt))))


