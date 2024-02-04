#lang racket
;---------------------------LISTS-----------
;1
(define (foldl* op acc lst)
  (if (null? lst)
      acc
      (foldl* op
              (op acc (car lst))
              (cdr lst))))

(define (foldr* op nv lst)
  (if (null? lst)
      nv
      (op (car lst)
          (foldr* op nv (cdr lst)))))

;
(define (length* l)
  (foldl* (lambda (x . lst) (+ x 1)) 0 l))

;3
(define (uniques l)
  (foldr* (lambda (x acc)
                 (if (member x acc)
                     acc
                     (cons x acc)))
          '()
          l))

;4
(define (sat-n? p? n l)
  (define (op x acc)
    (if (p? x)
        (+ 1 acc)
        acc))
  (= (foldl* op 0 l) n))

;5
(define (count-atoms l)
  (define (op x acc)
    (cond ((null? x) acc)
          ((list? x) (+ acc (count-atoms x)))
          (else (+ acc 1))))
  (foldl* op 0 l))
          

;6
(define (partition-i p? l)
  (define (op x acc)
    (if (p? x)
        (cons (cons (car acc)) (cdr acc))
        (cons (car acc) (cons (cdr acc)))))
  (foldl* op '() l))

;7
(define (avg h . t)
  (/ (apply + (cons h t)) (length (cons h t))))

;8
(define (compose f g) (lambda (x) (f (g x))))

(define (compose-all f . g)
  (if (null? g)
      f
      (compose f (apply compose-all g))))

;9
(define (conjoint p1? p2?) (lambda (x) (and (p1? x) (p2? x))))

(define (conjoint-all p? . ps)
  (if (null? ps)
      p?
      (conjoint p? (apply conjoint-all ps))))

;10
(define (exists? p? l) (foldl* (lambda (x acc) (or (p? x) acc)) #f l))

(define (map* f . l) ;l is a list of lists
  (if (or (null? l) (exists? null? l))
      '()
      (const (apply f (map car l)) (apply map* f (map cdr l)))))












