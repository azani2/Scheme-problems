#lang racket
;-------------------ALISTS-------------------
(define (make-alist f keys) (map (lambda (key) (cons key (f (key)))) keys))

(define (keys alist) (map car alist))

(define (values alist) (map cdr alist))

(define (del-assoc key alist) (filter (lambda (kv) (not (equal? (car kv) key))) alist))

(define (add-assoc key value alist) (cons (cons key value) (del-assoc key alist)))

(define (add-asooc* key value alist)
  (let ((new-kv (cons key value)))
    (cond ((null? alist) (list new-kv))
          ((eqv? (caar alist) (key)) (cons new-kv (cdr alist)))
          (else (cons (car alist) (add-assoc key value (cdr alist)))))))

(define (add-assoc** key value alist)
  (let ((new-kv (cons key value)))
    (if (assv key alist)
        (map (lambda (kv) (if (eq? (car kv) key) new-kv kv)) alist)
        (cons new-kv alist))))

(define (search p l)
  (and (not (null? l)) (or (p (car l)) (search p (cdr l)))))

(define (assv key alist)
  (search (lambda (kv) (and (eqv? (car kv) key) kv)) alist))

(define (all? p l)
  (not (search (lambda (el) (not (p el))) l)))

(define (all*? p l)
  (foldr (lambda (x y) (and x y) #t) (map p l)))


;-------------------GRAPHS-----------------------
(define vertices keys)

(define (children v g) (cdr (assv v g)))

(define (edge? u v g) (memv v (children u g)))

(define (map-children v f g) (map f (children v g)))

(define (search-child v f g) (search f (children v g)))

(define (childless g)
  (filter (lambda (v) (null? (children v g))) (vertices g)))

(define (parents v g)
  (filter (lambda (u) (edge? u v g)) (vertices g)))

(define (symmetric? g)
  (all? (lambda (u) (all? (lambda (v) (edge? u v g)) (children u g)) (vertices g))))


;------------------DFS--------------------------

(define (dfs-path u v g)
  (define (dfs-search path)
    (let ((current (car path)))
      (cond ((eqv? current v) (reverse path))
            ((memv current (cdr path)) #f)
            (else ((search-child current (lambda (child) (dfs-search (cons child path))) g))))))
      (dfs-search (list u)))

;------------------BFS--------------------------

(define (extend path g)
  (map-children (car path) (lambda (child) (cons child path)) g))

(define (remains-acyclic? path)
  (not (memv (car path) (cdr path))))

(define (extend-acyclic path)
  (filter remains-acyclic? (extend path)))

(define (bfs-path u v g) 'un)
  
;----------------TO DO: FINISH BFS-------------------





     