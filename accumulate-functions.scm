; recursive accumulate function
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

; function which sums the cubes of the numbers between a and b using recursive accumulate function
(define (sum-cubes a b)
  (accumulate + 0 (lambda (x) (* x x x)) a (lambda (x) (+ 1 x)) b))

; iterative accumulate function
(define (accumulate-iter combiner null-value term a next b)
  (define (helper current result)
    (if (> current b)
        null-value
        (helper (next current) (combiner result (term current)))))

  (helper a null-value))

; function which sums the cubes of the numbers between a and b using iterative accumulate function
(define (sum-cubes-iter a b)
  (accumulate + 0 (lambda (x) (* x x x)) a (lambda (x) (+ 1 x)) b))

; filter accumulate
(define (filter-accumulate combiner null-value term filter a next b)
   (if (> a b)
      null-value
      (combiner (if (filter a) (term a) null-value)
                (filter-accumulate combiner null-value term filter (next a) next b))))

; function which multiplies the squares of all odd numbers between a and b
(define (prod-odd-squares a b)
  (filter-accumulate * 1 (lambda (x) (* x x)) odd? a (lambda (x) (+ 1 x)) b))

; pow function
(define (pow x n)
  (accumulate * 1 (lambda (random-value) x) 1 (lambda (i) (+ 1 i)) n))

(define (id x) x)

; n!! funcion
(define (n!! n)
  (accumulate * 1 id ((lambda () (if (odd? n) 1 2))) (lambda (i) (+ 2 i)) n))
