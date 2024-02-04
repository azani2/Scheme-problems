#lang racket
;-----------------ALISTS----------------------

(define (make-alist fn keys)
  (map (lambda (key)
         (cons key (fn key))) keys))

(define (add-assoc key value alist)
  (cons (cons key value)
        alist))

(define (alist-keys alist)
  (map car alist))

(define (alist-values alist)
  (map cdr alist))

(define (alist-assoc key alist)
  (cond ((null? alist) '())
        ((equal? (caar alist) key) (cdar alist))
        (else (alist-assoc key (cdr alist)))))

(define (del-assoc key alist)
  (filter (lambda (alist-pair)
            (not (equal? (car alist-pair) key)))
          alist))

;-----------------STREAMS----------------------------

(define empty-stream '())

(define-syntax cons-stream
  (syntax-rules () ((cons-stream h t)
                    (cons h (delay t)))))

(define head car)

(define (tail s) (force (cdr s)))

(define (stream-take* n s)
  (if (or (zero? n) (null? s))
    '()
    (cons (head s)
          (stream-take* (- n 1) (tail s)))))

;-------------------------------------------------

;1
(define (iterate f x)
  (cons-stream x (iterate f (f x))))

;2
(define (cycle lst)
  (define (cycle-help lst-tmp)
    (if (null? lst-tmp)
        (cycle-help lst)
        (cons-stream (car lst-tmp) (cycle-help (cdr lst-tmp)))))
  (cycle-help lst))

;3
(define (stream-map* f s)
  (if (null? s)
      '()
      (cons-stream (f (head s)) (stream-map* f (tail s)))))

;4
(define (nats-help i) ;nats-from
    (cons-stream i (nats-help (+ 1 i))))

(define nats
  (nats-help 1))

;-------------------------GRAPHS-------------------------------

(define (make-graph vs)
  (map list vs))

(define empty-graph? null?)

(define vertices alist-keys)

(define (add-vertex v g)
  (cons (list v) g))
;--------------------------------------------------------
(define g '((1 2 3) (2 4) (3 4) (4 5) (5)))

;--------------------------------------------------------

;5
(define (edges g)
  (if (null? g)
      '()
      (append (map (lambda (x) (cons (caar g) x)) (cdar g))
            (edges (cdr g)))))

; teacher version
(define (children-edges vs)
  (map (lambda (v) (cons (car vs) v))
       (cdr vs)))

(define (edges* g)
  (apply append (map children-edges g)))

;7
(define (children v g) (alist-assoc v g))

;6
(define (edge? u v g)
  (member v (children u g)))

;8
(define (map-children v f g)
  (map f (children v g)))

;9
(define (search-child v p g)
  (define (search-help ch-lst)
  (cond ((null? ch-lst) '())
        ((p (car ch-lst)) (car ch-lst))
        (else (search-help (cdr ch-lst)))))
  (search-help (children v g)))

;10
(define (remove-vertex v g)
  (map (lambda (l)
         (filter (lambda (x) (not (equal? x v))) l))
       (del-assoc v g)))

;11
(define (add-if-missing x l)
  (if (member x l)
      l
      (cons x l)))

(define (add-assoc-if-missing key value alist)
  (if (member key (alist-keys alist))
      alist
      (add-assoc key value alist)))

(define (add-edge u v g)
  (let ((gvu (cond ((and (member u (vertices g)) (member v (vertices g)))
                    g)
                   ((and (not (member u (vertices g))) (member v (vertices g)))
                    (add-vertex u g))
                   ((and (not (member v (vertices g))) (member u (vertices g)))
                    (add-vertex v g))
                   (else (add-vertex v (add-vertex u g))))))
    (add-assoc u (add-if-missing v (children u gvu)) (del-assoc u gvu))))

;g -> ((1 2 3) (2 4) (3 4) (4 5) (5)))
(define g1 (add-edge 3 6 (add-edge 3 5 g)))
; -> '((3 6 5 4) (6) (1 2 3) (2 4) (4 5) (5))

;12
(define (remove-edge u v g)
  (add-assoc u
             (remove v (children u g))
             (del-assoc u g)))

;13
(define (degree v g)
  (length (children v g)))

;14
(define (every? p l)
  (or (null? l)
      (and (p (car l)) (every? p (cdr l)))))

(define (symmetric? g)
  (every? (lambda (x) (edge? (cdr x) (car x) g)) (edges g)))

(define g2 '((1 3 2) (3 1) (2 1)))

;15
(define (invert g)
  (foldl (lambda (edge inverted-g)
         (add-edge (cdr edge) (car edge) inverted-g))
         '()
         (edges g)))

;16
(define (search p l)
  (and (not (null? l))
       (or (p (car l))
           (search p (cdr l)))))

(define (path? u v g)
  (define (dfs path)
    (let ((current (car path)))
      (or (equal? current v)
          (and (not (member current (cdr path)))
               (search-child current
                             (lambda (child)
                               (dfs (cons child path)))
                             g)))))
    (not (search (lambda (vertex)
                 (dfs (list vertex)))
               (vertices g))))

(define g3 '((6) (1 5 4 3 2) (5) (4) (3 1) (2 1)))
;(path? 2 5 g3)
;#f (?)
;(path? 1 6 g3)
;#f (ok)

;17
(define (acyclic? g)
  (define (dfs path)
    (let ((current (car path)))
      (or (member current (cdr path))
          (search-child current
                        (lambda (child)
                          (dfs (cons child path)))
                        g))))
    (not (search (lambda (vertex)
                 (dfs (list vertex)))
               (vertices g))))
;(acyclic? (make-graph '(1 2 3 4)))
;#f (?)








