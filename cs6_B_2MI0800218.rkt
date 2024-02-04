#lang racket

(define-syntax cons-stream
  (syntax-rules () ((cons-stream h t)
                    (cons h (delay t)))))

(define head car)

(define (tail s) (force (cdr s)))

;-----------------------------------1-------------------------
;-----------------------------------A-------------------------
(define (1+ n) (+ 1 n))
(define (foldl op nv l)
  (if (null? l) nv
      (foldl op (op nv (car l)) (cdr l))))

(define (begins-with? l p)
    (cond ((and (null? l) (not (null? p))) #f)
          ((null? p) #t)
          ((not (eq? (car p) (car l))) #f)
          (else (begins-with? (cdr l) (cdr p)))))
      

(define (has-prefix-in? list prefixes)
  (cond ((null? prefixes) #f)
        ((begins-with? list (car prefixes)) #t)
        (else (has-prefix-in? list (cdr prefixes)))))

(define (listsWithPrefix l p)
  (define (help count lst)
    (cond ((null? lst) count)
          ((has-prefix-in? (car lst) p) (help (1+ count) (cdr lst)))
          (else (help count (cdr lst)))))
  (help 0 l))

;-----------------------------------Б-------------------------
(define (max-index list)
  (define (help max-el max-idx l i)
    (cond ((null? l) max-idx)
          ((> (car l) max-el) (help (car l) i (cdr l) (1+ i)))
          (else (help max-el max-idx (cdr l) (1+ i)))))
  (help 0 0 list 0))

(define (prefix-counts l lp)
  (map (lambda (p) (listsWithPrefix l p)) lp))

(define (minListsWithPrefix l lp)
  (let ((max-idx (max-index (prefix-counts l lp))))
    (define (help i plist)
      (if (= i max-idx)
          (car plist)
          (help (1+ i) (cdr plist))))
    (help 0 lp)))

;-------TESTS-----
(define lsts '((1 2 3) (2 3 4) (3 4 5)))
(define plist '((3) (2 4) (1 2)))
(listsWithPrefix lsts plist)
(define test-counts (prefix-counts lsts '(((5) (55) (555)) ((3) (2 4) (1 2)) ((3) (600 3) (4)))))
test-counts
(max-index test-counts)

(minListsWithPrefix lsts '(((5) (55) (555)) ((3) (2 4) (1 2)) ((3) (600 3) (4))))

;-------------------за 2:--------------------------

(define (alist-keys alist) (map car alist))

(define (alist-assoc key alist)
  (cond ((null? alist) '())
        ((equal? (caar alist) key) (cdar alist))
        (else (alist-assoc key (cdr alist)))))

(define (del-assoc key alist) (filter (lambda (kv) (not (equal? (car kv) key))) alist))

(define (search p l)
  (and (not (null? l)) (or (p (car l)) (search p (cdr l)))))

(define vertices alist-keys)

(define (edges g)
  (if (null? g)
      '()
      (append (map (lambda (x) (cons (caar g) x)) (cdar g))
            (edges (cdr g)))))
;-----------------------------------2-------------------------

;-----------------------------------A-------------------------
(define (memv? x l)
  (if (eq? #f (memv x l))
      #f
      #t))

(define (contains kv al)
  (if (and (memv? (car kv) (alist-keys al)) (eqv? (alist-assoc (car kv) al) (cdr kv)))
      #t
      #f))

(define (abs-edges edges-list)
  (define (help elist edges)
      (cond ((null? edges) elist)
            ((contains (cons (cdar edges) (caar edges)) elist) (help elist (cdr edges)))
            (else (help (cons (car edges) elist) (cdr edges)))))
  (help '() edges-list))

(define (included-in x alist)
  (define (help count al)
    (cond ((null? al) count)
          ((or (eq? (caar al) x) (eq? (cdar al) x)) (help (1+ count) (cdr al)))
          (else (help count (cdr al)))))
  (help 0 alist))

(define (countFriends g)
  (let ((relationships (abs-edges (edges g)))
        (people (vertices g)))
    (define (help p)
      (if (null? p)
          '()
          (cons (cons (car p) (included-in (car p) relationships)) (help (cdr p)))))
    (help people)))


;----------TESTS----------
(define g '((1 2 3) (2 4) (3 4) (4 2 5) (5)))
(define e (edges g))
(abs-edges e)
(countFriends g)

;-----------------------------------3-------------------------
(define (get-distance p1 p2)
  (let ((x (- (car p1) (car p2)))
        (y (- (cdr p1) (cdr p2))))
    (sqrt (+ (* x x) (* y y)))))

(define (move-x s point)
  (cons (+ s (car point)) (cdr point)))

(define (move-y s point)
  (cons (car point) (+ s (cdr point))))

(define (correct? p1 p2)
    (let ((dist (get-distance p1 p2)))
      (and (< dist 3) (> dist 2))))

(define (next-points step point-pair)
    (let ((p1 (car point-pair))
          (p2 (cdr point-pair)))
    ) 'un)

(define pointPairs 'un)

(define (get-n-list s n)
  (define (iter i str)
    (if (> i n)
        '()
        (cons (head str) (iter (1+ i) (tail str)))))
  (iter 1 s))

;------TESTS-----
;(get-n-list pointPairs 4)
(get-distance (cons 0 0) (cons 2 1))
(correct? (cons 0 0) (cons 2 1))











