;;for segments->painter
(load "segments-painter.scm")
;;for make-vect
(load "92-exercise-2.46.scm")
;;for make-segment
(load "93-exercise-2.48.scm")


;;(a)
(define bottom-left (make-vect 0.0 0.0))
(define bottom-right (make-vect 1.0 0.0))
(define top-left (make-vect 0.0 1.0))
(define top-right (make-vect 1.0 1.0))

(define left-segment (make-segment bottom-left top-left))
(define right-segment (make-segment bottom-right top-right))
(define top-segment (make-segment top-left top-right))
(define bottom-segment (make-segment bottom-left bottom-right))

(define (framepainter frame)
    ((segments->painter (list left-segment
 			      top-segment
			      right-segment
			      bottom-right))
      frame))

;;(b)

(define bottom-left--to--top-right (make-segment bottom-left top-right))
(define bottom-right--to--top-left (make-segment bottom-right top-left))

(define (diagonalpainter frame)
    ((segments->painter (list bottom-left--to--top-right
			      bottom-right--to--top-left))
     frame))

;;(c)
(define left-center (make-vect 0.0 0.5))
(define right-center (make-vect 1.0 0.5))
(define bottom-center (make-vect 0.5 0.0))
(define top-center (make-vect 0.5 1.0))

(define left-center--to--top-center (make-segment left-center top-center))
(define top-center--to--right-center (make-segment top-center right-center))
(define right-center--to--bottom-center (make-segment right-center bottom-center))
(define bottom-center--to--left-center (make-segment bottom-center left-center))

(define (prismaticpainter frame)
    ((segments->painter (list left-center--to--top-center 
			     top-center--to--right-center
			     right-center--to--bottom-center
			     bottom-center--to--left-center))
       frame))

;;(d)  copy from...
(segments->painter (list
                         (make-segment (make-vect 0.4 1.0)      ; 头部左上
                                       (make-vect 0.35 0.85))
                         (make-segment (make-vect 0.35 0.85)    ; 头部左下
                                       (make-vect 0.4 0.64))
                         (make-segment (make-vect 0.4 0.65)     ; 左肩
                                       (make-vect 0.25 0.65))
                         (make-segment (make-vect 0.25 0.65)    ; 左手臂上部
                                       (make-vect 0.15 0.6))
                         (make-segment (make-vect 0.15 0.6)     ; 左手上部
                                       (make-vect 0.0 0.85))

                         (make-segment (make-vect 0.0 0.65)     ; 左手下部
                                       (make-vect 0.15 0.35))
                         (make-segment (make-vect 0.15 0.35)    ; 左手臂下部
                                       (make-vect 0.25 0.6))

                         (make-segment (make-vect 0.25 0.6)     ; 左边身体
                                       (make-vect 0.35 0.5))
                         (make-segment (make-vect 0.35 0.5)     ; 左腿外侧
                                       (make-vect 0.25 0.0))
                         (make-segment (make-vect 0.6 1.0)      ; 头部右上
                                       (make-vect 0.65 0.85))
                         (make-segment (make-vect 0.65 0.85)    ; 头部右下
                                       (make-vect 0.6 0.65))
                         (make-segment (make-vect 0.6 0.65)     ; 右肩
                                       (make-vect 0.75 0.65))
                         (make-segment (make-vect 0.75 0.65)    ; 右手上部
                                       (make-vect 1.0 0.3))

                         (make-segment (make-vect 1.0 0.15)     ; 右手下部
                                       (make-vect 0.6 0.5))
                         (make-segment (make-vect 0.6 0.5)      ; 右腿外侧
                                       (make-vect 0.75 0.0))

                         (make-segment (make-vect 0.4 0.0)      ; 左腿内侧
                                       (make-vect 0.5 0.3))
                         (make-segment (make-vect 0.6 0.0)      ; 右腿内侧
                                       (make-vect 0.5 0.3))))
