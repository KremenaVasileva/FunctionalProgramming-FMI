(define (add3)
  (lambda (x) (+ x 3)))

; flipping the arguments of the f function
(define (flip f)
  (lambda (a b) (f b a)))

; curry of a function of 2 arguments
(define (curry-binary f)
  (lambda (x) (lambda (y) (f x y))))

; curry of a function of 3 arguments
(define (curry-binary f)
  (lambda (x) (lambda (y) (lambda (z) (f x y z)))))

; a function multiplying the odd numbers between a and b
(define (prod-odd a b)
  (cond ((> a b) 0)
        ((= a b) 1)
        (else (* ((lambda (x) (if (odd? x) x 1)) a)
                 (prod-odd (+ a 1) b)))))

; the accumulate function
(define (accumulate op nv term a next b)
  (if (> a b)
      nv
      (op (term a)
          (accumulate op nv term (next a) next b))))

; product of the squares of all odd numbers between a and b
(define (prod-odd-squares a b)
  (accumulate * 1 (lambda (x) (if (odd? x) (* x x) 1)) a (lambda (x) (+ 1 x)) b))

; iter accumulate
(define (accumulate-iter op nv term a next b)
  (define (helper current result)
    (if (> current b)
      result
      (helper (next current) (op (term current) result))))
  (helper a nv)
  )

; the prod-odd-squares function using the accumulate-iter function
(define (prod-odd-sq-iter a b)
  (accumulate-iter * 1 (lambda (x) (if (odd? x) (* x x) 1)) a (lambda (x) (+ 1 x)) b))

(define (id x) x)
(define (1+ x) (+ 1 x))
(define (2+ x) (+ 2 x))

(define (pow x n)
  (accumulate * 1
              (lambda (random) x)
              1
              1+
              n))

; returns n * (n - 2) * ... * 1
(define (n!! n)
  (accumulate *
            1
            id
            ((lambda () (if (odd? n) 1 2)))
            2+
            n
            ))

; accumulate op null_value term a next b
; sum of [x^i / (x+i)] where i = [1, 2, .. n]
(define (sum x n)
  (accumulate +
              0
              (lambda (i) (/ (expt x i) (+ x i)))
              1
              1+
              n))

; accumulate op null_value term a next b
(define (prime? x)
  (accumulate (lambda (x y) (if (and x y) #t #f))
              #t
              (lambda (i) (if (= (remainder x i) 0) #f #t))
              2
              1+
              (sqrt x)))
