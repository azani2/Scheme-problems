#lang racket

;---------------BINARY TREES-------------

(define (tree? t)
  (or (null? t)
      (and (list? t)
           (= (length t) 3)
           (tree? (cadr t))
           (tree? (caddr t)))))

(define root-tree car)

(define left-tree cadr)

(define right-tree caddr)

(define empty? null?)

(define empty-tree '())

(define (make-tree root left right) (list root left right))

(define (depth-tree t)
  (if (empty? t)
      0
      (+ 1 (max (depth-tree (left-tree t)) (depth-tree (right-tree t))))))

(define (memv-tree x t)
  (and ((not empty t)
        (or (and (eqv? x (root-tree t)) t)
            (memv-tree x (left-tree t))
            (memv-tree x (right-tree t))))))

(define (memv-tree* x t)
  (cond ((empty? t) #f)
        ((eqv? x (root-tree t)) #t)
        (else (or (memv-tree* x (left-tree t)) (memv-tree* x (right-tree t))))))

(define (cons#f h t) (and t (cons h t)))

(define (path-tree x t)
  (cond ((empty? t) #f)
        ((eqv? (root-tree t) x) (list x))
        (else (cons#f (root-tree t) (or (path-tree x (left-tree t))
                                        (path-tree x (right-tree t)))))))

(define (path-tree& x t)
  (and (not (empty? t))
       (or (and (eqv? (root-tree t)) (list x))
           (cons#f (root-tree t) (or (path-tree x (left-tree t))
                                        (path-tree x (right-tree t)))))))