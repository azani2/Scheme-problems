#lang racket

(define (foldr op nv lst)
        (if (null? lst)
            nv
            (op (car lst) (foldr op nv (cdr lst)))))

    (define (foldl op acc lst)
        (if (null? lst)
            acc
            (foldl op (op acc (car lst)) (cdr lst))))

    (define (filter pred? lst)
      (foldr (lambda (x nv)
               (if (pred? x)
                   (cons x nv)
                   nv))
             '()
             lst))

(define (accumulate from to term op acc)
  (if (> from to)
      acc
      (accumulate (+ from 1)
                  to
                  term
                  op
                  (op acc (term from)))))
    
;1
(define (cartesian-product l1 l2)
  (if (null? l1)
      '()
      (append (map (lambda (x) (cons (car l1) x)) l2) (cartesian-product (cdr l1) l2))))
 
;AYEE

;2

(define (id x) x)

(define (sum-interval-i from to)
  (accumulate from to id + 0))
  
(define (excluded n)
  (define sum-n (sum-interval-i 1 n))
  (define (iter i j)
    (cond ((> i n) '())
          ((> j n) (iter (+ i 1) 1))
          ((= (* i j) (- sum-n i j)) (append (cons i j) (iter i (+ j 1))))
          (else (iter i (+ j 1)))))
  (iter 1 1))


















        