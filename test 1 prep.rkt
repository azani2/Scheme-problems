#lang racket

;Variant 1
(define (accumulate from to term op acc)
  (if (> from to)
      acc
      (accumulate (+ from 1) to term op (op (term from) acc))))

(define (id x) x)
(define (my-and x y) (and x y))

;1
(define (prime? n)
  (define (not-divisible-by? x)
    (if (= (remainder n x) 0)
        #f
        #t))
  (accumulate 2 (quotient n 2) not-divisible-by? my-and #t))

(define (trim n)
  (define (trim-term x)
    (if (and (prime? x) (= (remainder n x) 0))
        x
        1))
  (define (trim-/ x n) (/ n x))
  (accumulate 2 n trim-term trim-/ n))

;2
(define (countCommonUnitary a b)
  (define (have-no-common-prime-divisor k q)
    (define (is-not-common-prime-divisor i)
      (if (and (prime? i) (= (remainder k i)0) (= (remainder q i) 0))
      #f
      #t))
    (accumulate 2 q is-not-common-prime-divisor my-and #t))
  (define (unitary? k n)
    (and (= (remainder n k) 0) (have-no-common-prime-divisor k (quotient n k))))
  (define (commonUnitary? k)
    (and (unitary? k a) (unitary? k b)))
  (define smaller-or-second
    (if (< a b)
        a
        b))
  (define (succ-if-found x acc)
    (if (commonUnitary? x)
        (+ acc 1)
        acc))
  (accumulate 1 smaller-or-second id succ-if-found 0))

;3
(define (min a b)
  (if (> a b)
      b
      a))

(define (max a b)
  (if (< a b)
      b
      a))

(define (selectiveMerge f l1 l2)
  (define (iter l1 l2 last)
    (cond ((or (null? l1) (null? l2)) '())
          ((< (f (car l1) (car l2)) (min (car l1) (car l2))) (cons (car l1) (iter (cdr l1) (cdr l2) 1)))
          ((> (f (car l1) (car l2)) (max (car l1) (car l2))) (cons (f (car l1) (car l2)) (iter (cdr l1) (cdr l2) 2)))
          ((= last 1) (cons (car l1) (iter (cdr l1) (cdr l2) 1)))
          (else (cons (f (car l1) (car l2)) (iter (cdr l1) (cdr l2) 2)))))
  (iter l1 l2 1))

;4
(define (foldl op nv l)
  (if (null? l)
      nv
      (foldl op (op nv (car l)) (cdr l))))
          
(define (my-member? x l)
  (if (list? (member x l))
      #t
      #f))

(define (intersect l1 l2)
  (define (cons-if-member acc x)
    (if (my-member? x l1)
        (append acc (list x))
        acc))
    (foldl cons-if-member '() l2))

(define (preferred-network supported options)
  (define (preferred-index currentIdx maxLength maxIdx l2)
    (cond ((null? l2) maxIdx)
          ((and (> (length (intersect supported (car l2))) 2) (> (length (intersect supported (car l2))) maxLength))
           (preferred-index (+ currentIdx 1) (length (intersect supported (car l2))) currentIdx (cdr l2)))
          (else (preferred-index (+ currentIdx 1) maxLength maxIdx (cdr l2)))))
  (define idx (preferred-index 0 -1 -1 options))
  (define (preferred-network-freqs i l2)
    (cond ((= idx -1) '())
          ((< i idx) (preferred-network-freqs (+ i 1) (cdr l2)))
          (else (intersect supported (car l2)))))
  ;idx) ;check if preferred-idx function works correctly
  (preferred-network-freqs 0 options))
          

;OK

;Variant 2
;2
(define (grow n)
  (define (op x acc)
    (if (and (prime? x) (= (remainder n x) 0))
        (* x acc)
        acc))
  (accumulate 1 (quotient n 2) id op n))

;2
(define (maxUnitary n)
  (define (have-no-common-prime-divisor k q)
    (define (is-not-common-prime-divisor i)
      (if (and (prime? i) (= (remainder k i)0) (= (remainder q i) 0))
      #f
      #t))
    (accumulate 2 q is-not-common-prime-divisor my-and #t))
  (define (unitary? k n)
    (and (= (remainder n k) 0) (have-no-common-prime-divisor k (quotient n k))))
  (define (iter i max)
    (cond ((= i n) max)
         ((and (unitary? i n) (> i max)) (iter (+ i 1) i))
         (else (iter (+ i 1) max))))
  (iter 1 -1))

;3
(define (selectiveMap f l1 l2)
  (define (iter l1 l2 last)
    (cond ((or (null? l1) (null? l2)) '())
          ((and (< (f (car l1)) (f (car l2))) (< (f (car l2)) (min (car l1) (car l2)))) (cons (f (car l1)) (iter (cdr l1) (cdr l2) 1)))
          ((and (> (f (car l1)) (max (car l1) (car l2))) (> (f (car l2)) (f (car l1)))) (cons (f (car l2)) (iter (cdr l1) (cdr l2) 2)))
          ((= last 1) (cons (f (car l1)) (iter (cdr l1) (cdr l2) 1)))
          (else (cons (f (car l2)) (iter (cdr l1) (cdr l2) 2)))))
  (iter l1 l2 1))

;4 is smeantically the same as variant 1's 4 but with changed name



