#lang racket

;---------------BINARY TREES------------

(define (tree? t)
  (or (null? t)
      (and (list? t)
           (= (length t) 3)
           (tree? (cadr t))
           (tree? (caddr t)))))

(define root car)

(define left cadr)

(define right caddr)

(define (make-tree root left right)
  (list root left right))

(define empty? null?)

(define (leaf? t)
  (and (empty? (left t))
       (empty? (right t))))

;-----------------ALISTS------------

(define (make-alist fn keys)
  (map (lambda (key)
         (cons key (fn key)))
       keys))

(define (add-assoc key value alist)
  (cons (cons key value)
        alist))

(define (alist-keys alist)
  (map car alist))

;--------------------------------------

;1
(define (count-leaves tree)
  (cond ((empty? tree) 0)
        ((leaf? tree) 1)
        (else (+ (count-leaves (left tree))
                 (count-leaves (right tree))))))

;2
(define (map-tree f tree)
  (if (empty? tree)
      '()
      (make-tree (f (root tree))
                 (map-tree (left tree))
                 (map-tree (right tree)))))

;3
(define (level n tree)
  (cond ((empty? tree) '())
        ((zero? n) (list (root tree)))
        (else (append (level (- n 1) (left tree))
                      (level (- n 1) (right tree))))))

;4
(define (pre-order tree)
  (if (empty? tree)
      '()
      (append (list (root tree)) (pre-order (left tree)) (pre-order (right tree)))))


(define t (make-tree '(1) '(2 (3 () ()) (4 () ())) '(5 (6 () ()) (7 () ()))))
;is tree? ok
(define t2 (make-tree 1 '(2 (3 () ()) (4 () ())) '(5 (6 () ()) (7 () ()))))
;is tree? ok too

;5
(define (in-order tree)
  (if (empty? tree)
      '()
      (append (in-order (left tree))
              (list (root tree))
              (in-order (right tree)))))

;6
(define (post-order tree)
  (if (empty? tree)
      '()
      (append (post-order (left tree))
              (post-order (right tree))
              (list (root tree)))))

;7
(define (flip-tree tree)
  (if (empty? tree)
      '()
      (make-tree (root tree)
                 (flip-tree (right tree))
                 (flip-tree (left tree)))))

;8
(define (alist-values alist)
  (map cdr alist))

;9
(define (alist-assoc key alist)
  (cond ((null? alist) '())
        ((equal? (caar alist) key) (cadr alist))
        (else (alist-assoc key (cdr alist)))))

;10
(define (del-assoc key alist)
  (filter (lambda (kv) (not (equal? (car kv) key))) alist))

;11
(define (foldl* op acc lst)
  (if (null? lst)
      acc
      (foldl* op
              (op acc (car lst))
              (cdr lst))))


(define (partition-i p? list)
  (define (op acc x)
      (if (p? x)
        (cons (cons x (car acc))
              (cdr acc))
        (cons (car acc)
              (cons x (cdr acc)))))
      (foldl* op '(()) list))

(define l '(1 2 3 4 5 6 7 8 9))

(define (group-by* f list)
  (let* ((fhead (f (if (null? list) 0 (car list))))
         (p? (lambda (x) (equal? (f x) fhead)))
         (parts (partition-i p? list)))
    (if (null? list)
      '()
      (cons (cons fhead (car parts))
            (group-by* f (cdr parts))))))

(define l1 '(-1 -2 -3 -4 1 2 3 4))
(define (square x) (* x x))



  