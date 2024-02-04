#lang racket

(define (id x) x)
(define (my-and a b) (and a b))
(define (my-or a b) (or a b))
(define (num-len n) (+ 1 (floor (log n 10))))

;1 - ACUMULATE ITERATIVE
(define (accumulate from to term op acc)
  (if (> from to)
      acc
      (accumulate (+ from 1) to term op (op (term from) acc))))

;2
(define (fact n)
  (accumulate 1 n id * 1))

;3
(define (for-all? from to p?)
  (accumulate from to p? my-and #t))

;4 - CHECK IF INTERVAL CONTAINS ELEMENT E FOR WHICH : P(E) = T
(define (exists? from to p?)
  (accumulate from to p? my-or #f))

;5
(define (count-p from to p?)
  (define (succ-if-p x acc) ;op takes 2 arguments - first term(x), then acc
    (if (p? x)
        (+ acc 1)
        acc))
  (accumulate from to id succ-if-p 0))

;6 - CHECK IF N IS PRIME
(define (prime? n)
  (define (n-not-divisible-by? p)
    (if (= (remainder n p) 0)
        #f
        #t))
  (accumulate 2 (quotient n 2) n-not-divisible-by? my-and #t))

;7 - COUNT PRIME NUMBERS IN INTERVEAL (FROM ; TO)
;opt 1:
(define (primes-1 from to)
  (define (succ-if-prime n acc)
    (if (prime? n)
        (+ acc 1)
        acc))
  (accumulate from to id succ-if-prime 0))

;opt 2:
(define (primes-2 from to)
  (count-p from to prime?))

;8
(define (meet-twice? f g a b)
  (define (meet? x) (= (f x) (g x)))
  (define (succ-if-meet x acc)
    (if (meet? a)
        (+ acc 1)
        acc))
  (>= 2 (accumulate a b id succ-if-meet 0)))

;9
(define (reverse-digits n)
  (define (rev x acc)
    (+ (* acc 10) (remainder (quotient n (expt 10 x)) 10)))
  (accumulate 0 (- (num-len n) 1) id rev 0))


;10
(define (count-palindromes a b)
  (define (is-palindrome? x)
    (= x (reverse-digits x)))
  (count-p a b is-palindrome?))

;11
(define (middle-digit n)
  (define (cut-digits x acc)
    (remainder (quotient acc 10) (expt 10 (- (floor (log acc 10)) 1))))
  (if (even? (num-len n))
      -1
      (accumulate 1 (quotient (num-len n) 2) id cut-digits n)))






    