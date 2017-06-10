#lang racket
(define empty-tree? null?)
(define root car)
(define left cadr)
(define right caddr)

(define (make-tree root left right)
  (list root left right))

(define T '(5
            (3
             (1 () ())
             (4 () ()))
            (10
             (7 () ())
             (8
              ()
              (9 () ())))))

; sums all the elements of the tree
; (sum-tree T) -> 47
(define (sum-tree t)
  (if (empty-tree? t)
      0
      (+ (root t) 
         (sum-tree (left t)) 
         (sum-tree (right t)))))

; returns the elements on level l of tree t
; (level T 2) -> '(1 4 7 8)
(define (level t l)
  (cond ((empty-tree? t) '())
        ((= 0 l) (list (root t)))
        (else (append (level (left t) (- l 1)) (level (right t) (- l 1))))))

; (level* T 2) -> '(1 4 7 8)
(define (level* t l)
  (define (helper t curr)
    (cond ((empty-tree? t) t)
          ((= l curr) (list (root t)))
          (else (append (helper (left t) (+ curr 1))
                        (helper (right t) (+ curr 1))))))
  (helper t 0))

; builds an expression tree
(define (build-expr-tree expr)
  (cond ((null? expr) '())
        ((number? expr) (make-tree expr '() '()))
        (else (make-tree (cadr expr)
                         (build-expr-tree (car expr))
                         (build-expr-tree (caddr expr))))))

; evaluates an expression tree
(define (eval-expr-tree t)
  (if (empty-tree? (left t))
      (root t)
      ((eval (root t)) (eval-expr-tree (left t)) (eval-expr-tree (right t)))))

(define expr1 '((7 + 4) - (2 * 3)))
(define expr2 '((42 - 6) / 6))

;(eval-expr-tree (build-expr-tree expr1)) -> 5
;(eval-expr-tree (build-expr-tree expr2)) -> 6

; maps fn to all the elements of t
; (tree-map T (lambda (x) (* x x))) -> '(25
;                                          (9
;                                            (1 () ())
;                                            (16 () ()))
;                                          (100
;                                              (49 () ())
;                                              (64 ()
;                                                  (81 () ()))))
(define (tree-map t fn)
  (if (empty-tree? t)
      '()
      (make-tree (fn (root t))
                 (tree-map (left t) fn)
                 (tree-map (right t) fn))))

;__________________________________________________
; Graphs
(define G '((1 2 3)
            (2 4)
            (3 5)
            (4)
            (5 1 6)
            (6)))

; (vertices G) -> '(1 2 3 4 5 6)
(define (vertices g)
  (map car g))

; (neighbours G 5) -> '(1 6)
(define (neighbours g v)
  (cdr (assoc v g)))

; (make-pairs 3 '(1 2 3 4 5)) -> '((3 . 1) (3 . 2) (3 . 3) (3 . 4) (3 . 5))
(define (make-pairs first others)
  (if (null? others)
      '()
      (cons (cons first (car others)) (make-pairs first (cdr others)))))

; (edges G) -> '((1 . 2) (1 . 3) (2 . 4) (3 . 5) (5 . 1) (5 . 6))
(define (edges g)
  (if (null? g)
      '()
      (append (make-pairs (caar g) (neighbours g (caar g)))
              (edges (cdr g)))))

(define (edges* g)
  (apply append (map (lambda (r)
                       (map (lambda (v) (cons (car r) v)) (cdr r))) g)))