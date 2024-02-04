#lang racket

(define-syntax cons-stream
  (syntax-rules () ((cons-stream h t)
                    (cons h (delay t)))))


(define (nats-help i)
    (cons-stream i (nats-help (+ 1 i))))

(define nats
  (nats-help 1))

(define (accumulate-i op nv a b term next)
  (if (> a b) nv
      (accumulate-i op (op nv (term a)) (next a) b term next)))

(define (id x) x)

(define head car)

(define (tail s) (force (cdr s)))


(define (round* precision x) (/ (round (* (expt 10 precision) x)) (expt 10 precision)))

(define (1+ n) (+ 1 n))

(define (foldl op nv l)
  (if (null? l) nv
      (foldl op (op nv (car l)) (cdr l))))

(define (sum list)
  (foldl + 0 list))

(define (get-first-n list n)
  (define (iter i l)
    (if (or (> i n) (null? l))
        '()
        (cons (car l) (iter (1+ i) (cdr l)))))
  (iter 1 list))

(define (get-n-list s n)
  (define (iter i str)
    (if (> i n)
        '()
        (cons (head str) (iter (1+ i) (tail str)))))
  (iter 1 s))

(define (movingAverage s n)
    (cons-stream (round* 2 (exact->inexact (/ (sum (get-n-list s n)) n)))
                 (movingAverage (tail s) n)))

;(get-n-list (movingAverage nats 2) 10)
;'(1.5 2.5 3.5 4.5 5.5 6.5 7.5 8.5 9.5 10.5)

(define (map-stream f . streams)
  (cons-stream (apply f (map head streams))
               (apply map-stream f (map tail streams))))

(define (stream-of s)
  (cons-stream s (stream-of s)))

(define (from i)
  (cons-stream i (from (1+ i))))

(define (allAverages s)
  (let ((s-streamed (stream-of s)))
    (map-stream movingAverage s-streamed (from 2))))

(define avgs (allAverages nats))
;(map (lambda (str) (get-n-list str 4)) (get-n-list avgs 4))