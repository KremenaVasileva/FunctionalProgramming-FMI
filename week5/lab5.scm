#lang racket
; our map implementation
; (map-func even? '(1 2 3 4)) -> '(#f #t #f #t)
(define (map-func f lst)
  (if (null? lst)
      '()
      (cons (f (car lst))
            (map f (cdr lst)))))

; ex.1
; (length* '(3 4 "hmm" #t #f)) -> 5
(define (length* lst)
  (if (null? lst)
      0
      (+ 1 (length* (cdr lst)))))

; (length** '(3 4 "hmm" #t #f)) -> 5
(define (length** lst)
  (define (helper lst result)
    (if (null? lst)
        result
        (helper (cdr lst) (+ 1 result))))
  (helper lst 0))

; (append 5 '(1 2 3))
; (list 1 2 3 4)

; (reverse* '(3 4 "hmm" #t #f)) -> '(#f #t "hmm" 4 3)
(define (reverse* lst)
  (if (null? lst)
      '()
      (append (reverse (cdr lst)) (list (car lst)))))

; (reverse** '(3 4 "hmm" #t #f)) -> '(#f #t "hmm" 4 3)
(define (reverse** lst)
  (define (helper lst res)
    (if (null? lst)
        res
        (helper (cdr lst) (cons (car lst) res))))
  (helper lst '()))

; ex.2
; (nth 3 '(0 1 2 3)) -> 3
(define (nth n lst)
  (cond ((null? lst) #f)
        ((= n 0) (car lst))
        (else (nth (- n 1) (cdr lst)))))

; ex.3
; (range 1 5) -> '(1 2 3 4 5)
(define (range from to)
  (if (> from to)
      '()
      (cons from (range (+ 1 from) to))))

; ex.4
(define (digit-list n)
  (if (< n 10)
      n
      (list (digit-list (quotient n 10)) (remainder n 10))))

; ex.5
(define (take* n lst)
  (cond ((null? lst) '())
        ((= n 0) '())
        (else (cons (car lst)
                    (take* (- n 1) (cdr lst))))))

;(take* 3 (range 5 10))

(define (drop* n lst)
  (cond ((null? lst) '())
        ((= n 0) lst)
        (else (drop* (- n 1) (cdr lst)))))

;(drop* 3 (range 5 10))

;zad6
(define (chunk n lst)
  (if (null? lst)
      '()
      (cons (take* n lst) (chunk n (drop* n lst)))))

;(chunk 4 (range 1 10))

;zad7
(define (all p? lst)
  (cond ((null? lst) #t)
        ((p? (car lst)) (all p? (cdr lst)))
        (else #f)))

;(all even? '(1 2 3 4 5))
;(all even? '(2 6 4 8))

(define (any p? lst)
  (cond ((null? lst) #f)
        ((p? (car lst)) #t)
        (else (any p? (cdr lst)))))

;(any (lambda (x) (> x 10)) '(4 2 6 3 1))

; with DeMorgan
(define (any* p? lst)
  (not (all (lambda (x) (not p? x))) lst))

; map, filter and reduce(fold)
; (foldr + 0 '(1 2 3 4 5)) -> right fold
; (foldl) -> reverse func?

(define (foldr* op nv lst)
  (if (null? lst)
      nv
      (op (car lst)
          (foldr* op nv (cdr lst)))))

;zad8
(define (zip lst1 lst2)
  (cond ((null? lst1) '())
        ((null? lst2) '())
        (else (cons (cons (car lst1) (car lst2)) (zip (cdr lst1) (cdr lst2))))))

;(zip '(1 2 3 4) '(#t #f #f))

;zad14
(define (my-arity . xs)
  (length xs))

;zad16
(define (group-by-f f lst)
  (combine-groups (group-pairs (map (lambda (x) (cons x (f x))) lst))))

(define (group-pairs lst)
  (define firsts (filter (lambda (x) (equal? (cdr (car lst))
                                          (cdr x))) lst))
  
  (define rest (filter (lambda (x) (not (equal? (cdr (car lst))
                                                (cdr x)))) lst))
  (if (null? lst)
      '()
      (cons firsts (group-pairs rest))))

(define (combine-groups lst)
  (map (lambda (l) (list (cdar l) (map car l))) lst))