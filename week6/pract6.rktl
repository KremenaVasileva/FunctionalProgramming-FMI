#lang racket
; returns a list with n's digits
; (list-digits 3224) -> (3 2 2 4)
(define (list-digits n)
  (define (helper n res)
    (if (= n 0)
        res
        (helper (quotient n 10) (cons (remainder n 10) res))))
  (helper n '()))

; returns a list with n's divisors
; (list-divisors 12) -> (1 2 3 4 6 12)
(define (list-divisors n)
  (define (helper current res)
    (cond ((= current 0) res)
          ((divides? current n) (helper (- current 1) (cons current res)))
          (else (helper (- current 1) res))))
  (helper n '()))

(define (divides? m n)
  (= 0 (remainder n m)))

; returns true if at least one of the elements of the list lst returns true for predicate?
;(any? odd? '(1 2 3 4)) -> #t
;(any? odd? '(2 6 4)) -> #f
(define (any? pred? lst)
  (cond ((null? lst) #f)
        ((pred? (car lst)) #t)
        (else (any? pred? (cdr lst)))))

; returns true if all the elements of the list lst return true for predicate?
;(all? even? '(2 6 4)) -> #t
;(all? even? '(2 6 5)) -> #f
(define (all? pred? lst)
  (cond ((null? lst) #t)
        ((not (pred? (car lst))) #f)
        (else (all? pred? (cdr lst)))))

; returns a list with the lengths of each list in lists
(define (lists-length . lists)
  (map length lists))

; higher-order function, maps the fns consequently to a list
; ((nmap 1+) '(1 2 2 3)) -> '(2 3 3 4)
; ((nmap 1+ 2+) '(1 2 2 3)) -> '(4 5 5 6)
(define (nmap . fns)
  (lambda (lst)
    (define (helper functions res)
      (if (null? functions)
          res
          (helper (cdr functions) (map (car functions) res))))
    (helper fns lst)))

(define (1+ x) (+ 1 x))
(define (2+ x) (+ 2 x))

; returns a list of pairs formed using l1 and l2
(define (zip l1 l2)
  (if (or (null? l1) (null? l2))
      '()
      (cons (cons (car l1) (car l2)) (zip (cdr l1) (cdr l2)))))

; insertion sort
; (insertion-sort '(1 3 2 13 14 10 3 6 5)) -> '(1 2 3 3 5 6 10 13 14)
(define (insertion-sort l)
  (define (helper current res)
    (if (null? current)
        res
        (helper (cdr current) (insert-sorted (car current) res))
        )
    )
   (helper l '()))


; inserts x in the sorted list l
(define (insert-sorted x l)
  (cond ((null? l) (list x))
        ((< x (car l)) (cons x l))
        (else (cons (car l) (insert-sorted x (cdr l))))))

; foldr example
(define (sum-even-sq lst)
  (foldr + 0 (filter even? (map sq lst))))

(define (sq x) (* x x))