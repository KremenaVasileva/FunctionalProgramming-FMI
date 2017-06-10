; sums the numbers in [0,x]
(define (sum x)
  (if (< x 1) x (+ x (sum (- x 1))))
  )

; the pow function
(define (pow x n)
  (if (= n 1) x (* x (pow x (- n 1)))))

(define (is-even? x)
  (= (remainder x 2) 0))

(define (square x)
  (* x x))

(define (fast-pow x n)
  (cond ((= n 1) x)
        ((is-even? n) (fast-pow (square x) (/ n 2)))
        (else (* x (fast-pow x (- n 1))))))

(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

; the factorial function using linear recursion
(define (fact-iter n)
  (define (fact-helper i result)
    (if (> i n)
        result
        (fact-helper (+ i 1) (* i result))))
  (fact-helper 1 1))

; sums the whole numbers in the range [a,b] using linear recursion
(define (sum-range a b)
  (define (sum-iter i result)
    (if (> i b)
        result
        (sum-iter (+ i 1) (+ i result))))
  (sum-iter a 0))

(define (reverse-digits x)
  (define (reverse-helper i result)
    (if (< i 10)
        (+ i (* result 10))
        (reverse-helper (quotient i 10) (+ (remainder i 10) (* result 10)))))
  (reverse-helper x 0))

; the fibonacci function using linear recursion
(define (fib-iter n)
  (define (fib-helper current next counter)
    (if (= counter n)
        current
        (fib-helper next (+ current next) (+ counter 1))))
  (fib-helper 0 1 1))

(define (is-prime? x)
  (define (is-prime-helper i)
    (cond ((> i (sqrt x)) #t)
          ((= (remainder x i) 0) #f)
          (else (is-prime-helper (+ i 1)))))
  (is-prime-helper 2))

(define znamenatel (/ (fact-iter 366) (* (fact-iter 186) (fact-iter 180))))
(define nechetni (/ (fact-iter 31) (* (fact-iter 16) (fact-iter 15))))
(define chetni (/ (fact-iter 30) (* (fact-iter 15) (fact-iter 15))))
(define fev (/ (fact-iter 29) (* (fact-iter 14) (fact-iter 15))))

(define chislitel (* (fast-pow nechetni 7) (fast-pow chetni 4) fev))

(define result (/ chislitel znamenatel))