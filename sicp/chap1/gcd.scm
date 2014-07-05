(define (mygcd a b)
  (if (= b 0)
      a
      (mygcd b (remainder a b))))
