; basic list functions
(define l1 '(1 2 3 4 5))
(define l2 '(1 3.14 "hi" #f (3 . 7)))

(define l3 (cons l1 l2))

(define (len list)
  (if (null? list)
      0
      (+ 1 (len (cdr list)))))

(define (member? x list)
  (cond ((null? list) #f)
        ((equal? x (car list)) #t)
        (else (member? x (cdr list)))))

(define (elem-at i list)
  (if (= i 0)
      (car list)
      (elem-at (- i 1) (cdr list))))

; reverses a list
(define (rev list)
  (define (helper current res)
    (cond ((null? current) res)
           (else (helper (cdr current) (cons (car current) res)))))
  (helper list '()))

; insert at the end
(define (insert x list)
  (if (null? list)
      (cons x '())
      (cons (car list)
            (insert x (cdr list)))))

; insert at the end
(define (insert* x list)
  (rev (cons x (rev list))))

; map
(define (my-map f list)
  (if (null? list)
      '()
      (cons (f (car list))
            (my-map f (cdr list)))))

;(my-map (lambda (x) (+ 2 x)) l1)