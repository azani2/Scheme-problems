#lang racket

;Lab 2

(define (plus2 n) (+ n 2))

(define (compose g f)
  (lambda (x) (f (g x))))

(define (id x) x)

(define (repeat f n)
  ( if (zero? n)
       id
       (lambda (x)
          ((repeat f (- n 1)) (f x))))) ;primitivna rekursiq

(define (is-even? n) (or (zero? n) (is-odd? (- n 1))))
(define (is-odd?  n) (and (not (zero? n)) (is-even? (- n 1))))

;tasks
;1 - recursive - NUMBER LENGTH
(define (count-digits n)
  (if (< n 1)
      0
      (+ (count-digits (/ n 10)) 1)))

;2 - recursive - POW(X , N)
(define (pow x n)
  (if (zero? n)
      1
      (* x (pow x (- n 1)))))

;3 - recursive - SUM OF INTERVAL (A , B)
(define (interval-sum a b)
  (if (> a b)
      0
      (if (= a b)
          b
          (+ a (interval-sum (+ a 1) b)))))

;4 - iterative - POW(X , N)
(define (pow-i x n)
  (define (iter product i)
    (if (zero? i)
        product
        (iter (* x product) (- i 1))))
  (iter 1 n))
        
;5 - iterative - NUMBER LENGTH
(define (count-digits-i n)
  (define (iter count n)
    (if (< n 1)
        count
        (iter (+ count 1) (/ n 10))))
  (iter 0 n))

;6 - iterative - SUM OF INTERVAL (A , B)
(define (interval-sum-i a b)
  (define(iter a b sum)
    (if (= a b)
        (+ sum a)
        (iter (+ a 1) b (+ a sum))))
(iter a b 0))

;7 - iterative - REVERSE DIGITS
(define (reverse-digits-i n)
  (define (iter result n)
    (if (zero? n)
        result
        (iter (+ (* result 10) (remainder n 10)) (quotient n 10)))) ;we use quotient for integer division
(iter n))

;8
(define (fast-pow x n)
  (define (iter x n result)
    (cond ((zero? n) result)
          ((even? n) (iter (* x x) (/ n 2) result))
          (else (iter x (- n 1)(* result x)))))
  (if (negative? n)
      (/ 1 (iter x (- n) 1))
      (iter x n 1)))










  