#lang racket
; basic matrix functions
(define M '((1 2 3) (4 5 6) (7 8 9)))

(define get-rows-count length)

(define (get-cols-count m) 
  (if (null? m)
      0
      (length (car m))))

(define get-row list-ref)

(define (get-col m i)
  (map (lambda (row) (list-ref row i)) m))

(define (transpose m)
  (if (null? (car m))
      '()
      (cons (map car m) (transpose (map cdr m)))))

(define (transpose* m)
  (define (helper i)
    (if (= i (get-rows-count m))
        '()
        (cons (get-col m i) (helper (+ i 1)))))
  (helper 0))

(define (delete-row m i)
  (cond ((null? m) '())
        ((= i 0) (cdr m))
        (else (cons (car m) (delete-row (cdr m) (- i 1))))))

(define (delete-col m i)
  (transpose (delete-row (transpose m) i)))

(define (delete-col* m i)
  (define (delete-helper idx l)
    (cond ((null? l) l)
          ((= i idx) (cdr l))
          (else (cons (car l) (delete-helper (+ 1 idx) (cdr l))))))
  (delete-helper 0 m))

; returns a list with the elements from the second diagonal
; (first-diag M) -> '(1 5 9)
(define (first-diag m)
  (if (null? m)
      '()
      (cons (caar m) (first-diag (cdr (map cdr m))))))

; (first-diag* M) -> '(1 5 9)
(define (first-diag* m)
  (define (helper i)
    (if (= i (get-rows-count m))
        '()
        (cons (get-element m i i) (helper (+ 1 i)))))
  (helper 0))

(define (get-element m i j)
  (list-ref (get-row m i) j))

; returns a list with the elements from the second diagonal
; (second-diag M) -> '(3 5 7)
(define (second-diag m)
  (define (helper i)
    (if (< i 0) ;(get-rows-count m)
        '()
        (cons (get-element m (- (length m) (+ 1 i)) i) (helper (- i 1)))))
  (helper (- (length m) 1)))

; sums the elements of both diagonals
; (sum-diag M) -> 30
(define (sum-diag m)
  (foldr + 0
         (append (first-diag m)
                 (second-diag m))))

; sums the elements of a list
(define (sum lst)
  (if (null? lst)
      0
      (+ (car lst) (sum (cdr lst)))))



;____________________________________________________________________
; TREES

(define T '(5
            (3
             (1 () ())
             (4 () ()))
            (10
             (7 () ())
             (8 ()
                (9 () ())))))

(define empty-tree? null?)

(define (make-tree root left right)
  (list root left right))

(define (root t)
  (car t))

(define (left-tree t)
  (if (empty-tree? t)
      '()
      (cadr t)))

(define (right-tree t)
  (if (empty-tree? t)
      '()
      (caddr t)))

(define (is-leaf? t)
  (and (not (empty-tree? t))
       (empty-tree? (left-tree t)) 
       (empty-tree? (right-tree t))))

(define (height t)
  (cond ((empty-tree? t) 0)
        ((is-leaf? t) 1)
        (else (max (+ 1 (height (left-tree t))) (+ 1 (height (right-tree t)))))))

(define (contains? t x)
  (cond ((empty-tree? t) #f)
        ((equal? x (car t)) #t)
        (else (or (contains? (left-tree t) x)
                  (contains? (right-tree t) x)))))

; in-order traversal
; (in-order T) -> '(1 3 4 5 7 10 8 9)
(define (in-order t)
  (if (empty-tree? t) 
      '()
      (append (in-order (left-tree t))
              (list (root t))
              (in-order (right-tree t)))))

; post-order traversal
; (post-order T) -> '(1 4 3 7 9 8 10 5)
(define (post-order t)
  (if (empty-tree? t) 
      '()
      (append (post-order (left-tree t))
              (post-order (right-tree t))
              (list (root t)))))

; pre-order traversal
; (pre-order T) -> '(5 3 1 4 10 7 8 9)
(define (pre-order t)
  (if (empty-tree? t) 
      '()
      (append (list (root t))
              (pre-order (left-tree t))
              (pre-order (right-tree t)))))