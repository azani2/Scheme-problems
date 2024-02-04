#lang racket

;-------------------DEEP LISTS------------

(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

(define (count-atoms l)
  (cond ((null? l) 0)
        ((atom? l) 1)
        (else (+ (count-atoms (car l))
                 (count-atoms (cdr l))))))

(define (flatten l)
  (cond ((null? l) '())
        ((atom? l) l)
        (else (append (flatten (car l)) (flatten (cdr l))))))

(define (deep-reverse l)
  (cond ((null? l) '())
        ((atom? l) l)
        (else (append (deep-reverse (cdr l)) (list (deep-reverse (car l)))))))

(define (deep-foldr op term nv l)
  (cond ((null? l) nv)
        ((atom? l) (term l))
        (else (op (deep-foldr op term nv (car l)) (deep-foldr (op term nv (cdr l)))))))

(define (count-atoms* l)
  (deep-foldr + (lambda (el) 1) 0 l))

(define (flaten* l)
  (deep-foldr append list '() l))

(define (snoc x l) (append l (list x)))

(define (id x) x)

(define (deep-reverse* l) (deep-foldr snoc id '() l))

(define (branch p? f g) (lambda (x) (p? x) (f x) (g x)))

(define (deep-foldr* op term  nv l)
  (foldr op nv (map (branch atom? term (lambda (el) (deep-foldr* op term nv l)) l))))