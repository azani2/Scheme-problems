#lang racket


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
            (not (eqv? (car alist-pair) key)))
          alist))

(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

(define (flatten l)
  (cond ((null? l) '())
        ((atom? l) (list l))
        (else (append (flatten (car l)) (flatten (cdr l))))))

(define (memv? x list)
  (if (eqv? #f (memv x list))
      #f
      #t))
;-----------------------------ALL OBJECTS-------------------------

(define (allObjects al)
    (filter (lambda (x) (not (memv? x (alist-keys al)))) (flatten al)))

(define inv (list (list 'docs (list 'ids 'invoices)) (list 'ids (list 'passport)) (list 'invoices '()) (list 'memes '())
                                          (list 'family (list 'new-year 'birthday)) (list 'funny (list 'memes)) (list 'pics (list 'family 'funny))))

;---------------------------CLEAN UP--------------------------------

(define (foldl op nv l)
  (if (null? l) nv
      (foldl op (op nv (car l)) (cdr l))))

(define (my-or x y) (or x y))

(define (filter-d p? dlist)
  (cond ((null? dlist) '())
        ((list? (car dlist)) (cons (filter-d p? (car dlist)) (filter-d p? (cdr dlist))))
        ((p? (car dlist)) (cons (car dlist) (filter-d p? (cdr dlist))))
        (else (filter-d p? (cdr dlist)))))
;  (cond ((null? dlist) '())
;        ((and (atom? dlist) (p? dlist)) dlist)
;        (else (append (filter p? (car dlist)) (list (filter p? (cdr dlist)))))))
;

(define (label? x al) (memv? x (alist-keys al)))

(define (empty-box? al l) (and (label? (car l) al) (equal? '(()) (cdr l))))

(define (cleanUp al)
  (define (remove-label label l) (cleanUp (filter-d (lambda (x) (not (eqv? label x))) (del-assoc label l))))
  (define (help boxes-to-check cleaned-list)
    (let ((box (if (null? boxes-to-check) '() (car boxes-to-check))))
    (cond ((null? boxes-to-check) cleaned-list)
          ((empty-box? al box) (remove-label (car box) cleaned-list))
          (else (help (cdr boxes-to-check) cleaned-list)))))
  (help al al))
         
(cleanUp inv)
(map (lambda (x) (empty-box? inv x)) inv)
(empty-box? inv (list 'invoices '()))
  
;  ;(define (contains-labels? l) (foldl (lambda (nv el) (if (label? el) (or #t nv) (or #f nv))) #f l))
;  (define (clean-box l)
;    (cond ((null? l) '())
;          ((label? (car l)) (cons (clean-box (alist-assoc (car l) al)) (clean-box (cdr l))))
;          ((and (= 1 (length l)) (not (label? l))) l)
;  (define (help l)
;    (cond ((label? l) (clean-box

                       
;-------------WHAT'S SUPPOSED TO HAPPEN-------------------------
;> (filter-d (lambda (x) (not (eqv? 'memes x))) (del-assoc 'memes inv))
;'((docs (ids invoices)) (ids (passport)) (invoices ()) (family (new-year birthday)) (funny ()) (pics (family funny)))
;> (define no-memes (filter-d (lambda (x) (not (eqv? 'memes x))) (del-assoc 'memes inv)))
;> no-memes
;'((docs (ids invoices)) (ids (passport)) (invoices ()) (family (new-year birthday)) (funny ()) (pics (family funny)))
;> (filter-d (lambda (x) (not (eqv? 'invoices x))) (del-assoc 'invoices no-memes))
;'((docs (ids)) (ids (passport)) (family (new-year birthday)) (funny ()) (pics (family funny)))
;> (define no-invoices (filter-d (lambda (x) (not (eqv? 'invoices x))) (del-assoc 'invoices no-memes)))
;> no-invoices
;'((docs (ids)) (ids (passport)) (family (new-year birthday)) (funny ()) (pics (family funny)))
;> (filter-d (lambda (x) (not (eqv? 'funny x))) (del-assoc 'funny no-invoices))
;'((docs (ids)) (ids (passport)) (family (new-year birthday)) (pics (family)))
;> (define no-funny (filter-d (lambda (x) (not (eqv? 'funny x))) (del-assoc 'funny no-invoices)))
;> no-funny
;'((docs (ids)) (ids (passport)) (family (new-year birthday)) (pics (family)))




