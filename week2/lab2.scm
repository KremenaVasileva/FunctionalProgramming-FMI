; defining log 10 using ln
(define (log10 n) (/ (log n) (log 10)))

; (count-digits 123) -> 3.0
(define (count-digits n)
  (if (= n 0)
      1
      (ceiling (log10 n))))

; regular reverse-int function
(define (reverse-int n)
  (if (< n 10)
      n
      (+ (* (remainder n 10)
            (expt 10 (- (count-digits n) 1)))
         (reverse-int (quotient n 10)))))

; reverse-int function using tail recursion
(define (reverse-int* n)
  (define (helper n result)
    (if (= n 0)
        result
        (helper (quotient n 10) (+ (* result 10) (remainder n 10)))))
  (helper n 0))

(define (palindrome? n)
  (if (= n (reverse-int n))
      #t
      #f))

(define (divides? n m)
  (if (= 0 (remainder n m))
      #t
      #f))

(define (divisors-sum n)
  (define (helper current result)
    (cond ((= current n) (+ result n))
          ((divides? n current) (helper (+ current 1) (+ result current)))
          (else (helper (+ current 1) result))))
  
  (helper 1 0))

(define (perfect? n)
  (if (= n (- (divisors-sum n) n))
      #t
      #f))

(define (divisors-number n)
  (define (helper current result)
    (cond ((= current n) (+ result 1))
          ((divides? n current) (helper (+ current 1) (+ result 1)))
          (else (helper (+ current 1) result))))
  
  (helper 1 0))

(define (prime? n)
  (= 2 (divisors-number n)))

(define (bigger? n m)
  (> n m))

; (increasing? 123) -> #t
; (increasing? 1232) -> #f
(define (increasing? n)
  (define (helper current)
    (cond ((= 0 (quotient current 10)) #t)
          ((not (bigger? (remainder current 10) (remainder (quotient current 10) 10))) #f)
          (else (helper (quotient current 10)))))
  (helper n))

; (to-binary 6) -> 110
(define (to-binary n)
  (if (= n 0)
      0
      (+ (* 10 (to-binary (quotient n 2)))
         (remainder n 2))))

; (to-binary* 6) -> 110
(define (to-binary* n)
  (define (helper current res i)
    (cond ((= current 0) res)
          (else (helper (quotient current 2)
                        (+ res (* (remainder current 2) (expt 10 i))) (+ i 1)))))
  
  (helper n 0 0))