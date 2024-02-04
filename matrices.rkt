#lang racket

(define (accumulate op nv from to term next) 'un)

;---------------------MATRICES---------------

(define (all? p? l)
  (foldr (lambda (x y) (and x y) #t (map p? l))))

(define (matrix? m)
  (and (list? m)
       (not (null? (car m)))
       (all? list? m)
       (all? (lambda (row) (= (length row) (length (car m)))) m)))

(define get-rows-count length)

(define (get-columns-count m) (length (car m)))

(define first-row car)

(define (first-column m) (map car m))

(define del-firs-row cdr)

(define (del-first-col m) (map cdr m))

(define get-row list-ref)

(define (get-column m idx)
  (map (lambda (row) (list-ref idx)) m))

(define (transpose m)
  (if (null? (first-row m))
      '()
      (cons (first-column m) (transpose (del-first-col m)))))

(define (transpose* m)
  (accumulate cons '() 0 (- (get-columns-count m) 1) (lambda (idx) (get-column m idx)) (lambda (n) (+ 1 n))))

(define (+vectors v1 v2) (map + v1 v2))

(define (+matrices m1 m2) (map +vectors m1 m2))

(define (*vectors v1 v2) (apply + (map * v1 v2)))

(define (*matrices m1 m2)
  (let ((m2t (transpose m2)))
    (map (lambda (row) (map (lambda (col) (*vectors row col)) m2t) m1))))
