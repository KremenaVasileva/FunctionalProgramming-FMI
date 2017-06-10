; returns init * a * (a + 1) * ... * b
(define (prod init a b)
  (define (helper current res)
    (if (> current b)
        (* init res)
        (helper (+ current 1) (* current res))))
  (helper a 1))

; reverse-int function using tail recursion
(define (reverse-int n)
  (define (helper n result)
    (if (= n 0)
        result
        (helper (quotient n 10) (+ (* result 10) (remainder n 10)))))
  (helper n 0))

(define (palindrome? n)
  (= n (reverse-int n)))

(define (divides? m n)
  (= 0 (remainder n m)))

(define (perfect? x)
  (define (divisor-sum x)
    (define (helper current res)
      (cond ((= current x) res)
            ((not (divides? current x)) (helper (+ current 1) res))
            (else (helper (+ current 1) (+ res current)))))

    (helper 1 0))

  (= (divisor-sum x) x))

(define (circle-area r)
  (let ((pi 3.141592))
    (* pi r r)))

(define (circle-perimeter r)
  (let ((pi 3.141592))
    (* 2 pi r)))

(define (S a b c)
  (let ((p (/ (+ a b c) 2)))
    (sqrt (* p (- p a) (- p b) (- p c)))))

(define (S* a b c)
  (let* ((P (+ a b c))
         (p (/ P 2)))
    (sqrt (* p (- p a) (- p b) (- p c)))))

; solves ax^2 + bx + c = 0
(define (solve a b c)
  (let* ((4ac (* 4 a c))
         (2a (* 2 a))
         (D (- (* b b) 4ac))
         (x1 (/ (+ (- b) (sqrt D)) 2a))
         (x2 (/ (- (- b) (sqrt D)) 2a)))
    (list x1 x2)))

(define pi
  (lambda () 3.1415))

(define (double x)
  (* 2 x))

(define (apply-twice f x)
  (f (f x)))

; returns: f(a) + f(a + 1) + ... + f(b)
(define (sum f a b)
  (define (sum-helper current res)
    (if (> current b)
        res
        (sum-helper (+ current 1) (+ res (f current)))))
  (sum-helper a 0))

(define (cube x)
  (* x x x))

(define (sum-cubes a b)
  (sum cube a b))

; composition of functions
(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (double x)
  (* 2 x))