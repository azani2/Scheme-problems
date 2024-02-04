#lang racket

;1
;recursive
(define (length* l)
  (if (null? l)
      0
      (+ 1 (cdr l))))

;iterative
(define (length-i* l)
  (define (iter l n)
    (if (null? l)
        n
        (iter (cdr l) (+ 1 n))))
  (iter l 0))

;2
(define (take* n l)
  (if (or (null? l) (zero? n))
      '()
      (cons (car l) (take* (- n 1) (cdr l)))))

;3
(define (drop* n l)
  (if (or (zero? n) (null? l))
      l
      (drop* (- n 1) (cdr l))))

;4
(define (from-to a b)
  (if (= a b)
      '()
      (cons a (from-to (+ a 1) b))))

;5
(define (sum l)
  (if (null? l)
      0
      (+ (car l) (sum (cdr l)))))

;6
(define (last* l)
  (if (null? (cdr l))
      (car l)
      (last* (cdr l))))

;7
(define (nth n l)
  (define (iter i l)
    (cond ((null? l) ('index-out-of-bounds))
          ((= i n) (car l))
          (else (iter (+ 1 i) (cdr l)))))
  (iter 0 l))

;8
(define (zip l1 l2)
  (if (or (null? l1) (null?l2))
      '()
      (cons ((car l1) (car l2)) (zip (cdr l1) (cdr l2)))))

;9
(define (append* l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (append* (cdr l1) l2))))

;10
(define (filter* p l)
  (cond ((null? l) '())
        ((p (car l)) (cons (car l) (filter* p (cdr l))))
        (else (filter* p (cdr l)))))

;11
(define (map* f l)
  (if (null? l)
      '()
      (const (f (car l)) (map* f (cdr l)))))

;12
(define (foldr* op nv l)
  (if (null? l)
      nv
      (op (car l) (foldr* op nv (cdr l)))))

;13
(define (foldl* op acc l)
  (if (null? l)
      acc
      (foldl* op (op (car l) acc) (cdr l))))

;deep foldr
(define (atom? x) (and (not (null? x)) (not (pair? x))))

(define (deep-foldr op term nv l)
  (cond ((null? l) nv)
        ((atom? l) (term l))
        (else (op (deep-foldr op term nv (car l)) (deep-foldr op term nv (cdr l))))))

(define (deep-foldl op term acc l)
  (cond ((null? l) acc)
        ((atom? l) (term l))
        (else (deep-foldl op term (op (car l) (deep-foldl op term acc l)) (cdr l)))))


























