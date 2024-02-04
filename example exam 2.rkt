#lang racket
(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate op nv (next a) b term next))))

(define (accumulate-i op nv a b term next)
  (if (> a b) nv
      (accumulate-i op (op nv (term a)) (next a) b term next)))

(define (filter p l)
  (cond ((null? l) l)
        ((p (car l)) (cons (car l) (filter p (cdr l))))
        (else ((filter p (cdr l))))))

(define (foldr op nv l)
  (if (null? l) nv
      (op (car l) (foldr op nv (cdr l)))))

(define (foldl op nv l)
  (if (null? l) nv
      (foldl op (op nv (car l)) (cdr l))))

(define (foldr1 op l)
  (if (null? (cdr l)) (car l)
      (op (car l) (foldr1 op (cdr l)))))

(define (foldl1 op l)
  (foldl op (car l) (cdr l)))

(define-syntax cons-stream
  (syntax-rules () ((cons-stream h t)
                    (cons h (delay t)))))

(define head car)

(define (tail s) (force (cdr s)))

(define (map-stream f . streams)
  (cons-stream (apply f (map head streams)) (apply map-stream f (map tail streams))))

(define (filter-stream p? s)
  (if (p? (head s))
      (cons-stream (head s) (filter-stream p? (tail s)))
      ((filter-stream p? (tail s)))))


;--------------------------------1---------------------------
(define (nats-help i)
    (cons-stream i (nats-help (+ 1 i))))

(define nats
  (nats-help 1))

(define the-empty-stream '())

(define (id x) x)

(define (isNPerm f n)
    (define (perm-iter i values)
      (cond ((> i (- n 1)) #t)
            ((or (memv (f i) values) (> (f i) (- n 1)) (< (f i) 0)) #f)
            (else (perm-iter (+ i 1) (cons (f i) values)))))
    (perm-iter 0 '()))

;(isNPerm (lambda (x) (if (odd? x) (- x 1) (+ x 1))) 10)
;#t

;(isNPerm (lambda (x) (if (odd? x) (- x 1) (+ x 1))) 9)
;#f

;(isNPerm (lambda (x) (remainder (- 3 x) 3)) 3)
;#t

;(isNPerm (lambda (x) (remainder (+ 2 x) 10)) 10)
;#t
(define (memv? el list)
  (if (not (memv el list))
      #f
      #t))

(define (maxCycle f n)
    (define (iter start current chain max-len max-cycle)
      (let ((next-start (+ start 1))
            (f-next-start (f (+ start 1)))
            (next (f current)))
      (cond ((> start n) max-cycle)
            ((or (> current n) (< current 0)) (iter next-start f-next-start (list next-start) max-len max-cycle))
            ((memv? current (reverse chain)) (if (> (length (memv current chain)) max-len)
                                      (iter next-start f-next-start (list next-start) (length (memv current (reverse chain))) (memv current (reverse chain)))
                                      (iter next-start f-next-start (list next-start) max-len max-cycle)))
            (else (iter start next (cons current chain) max-len max-cycle)))))
  (iter 0 (f 0) '(0) 0 '()))
            
;(maxCycle (lambda (x) (remainder (+ 2 x) 10)) 10)
;'(0 2 4 6 8)
;(maxCycle (lambda (x) (remainder (+ x 3) 10)) 10)
;'(0 3 6 9 2 5 8 1 4 7)


;(define n (exact->inexact (/ 1 3)))
(define (round* precision x) (/ (round (* (expt 10 precision) x)) (expt 10 precision)))

(define (1+ n) (+ 1 n))

(define (sum list)
  (accumulate-i + 0 0 (length list) id 1+))

(define (get-first-n list n)
  (define (iter i l)
    (if (or (> i n) (null? l))
        '()
        (cons (car l) (iter (1+ i) (cdr l)))))
  (iter 1 list))

(define (movingAverage s n)
  (define (avg-help current-n str)
    (let ((current-avg (round* 1 (exact->inexact (/ (sum current-n) n))))
          (str-remain (tail str)))
      (if (< (length current-n) (- n 1))
          (avg-help (cons (head str) current-n) str-remain)
          (cons-stream current-avg (avg-help (cons (head str) (get-first-n current-n (- n 1))) str-remain)))))
  (avg-help '() s))


(define (get-n-list s n)
  (define (iter i str)
    (if (> i n)
        '()
        (cons (head str) (iter (1+ i) (tail str)))))
  (iter 1 s))

(define s1 (movingAverage nats 2))
