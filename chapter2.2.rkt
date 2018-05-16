#lang racket
;2.21, 2.22, 2.23, 2.24, 2.25, 2.26


;book

(define (list-ref list n)
  (if (= n 0)
      (car list)
      (list-ref (cdr list) (- n 1))))


(define (length list)
  (define (iter a count)
    (if (null? a)
        count
        (iter (cdr a) (+ 1 count))))
    (iter list 0))

(define (len list)
  (if (null? list)
      0
      (+ 1 (len (cdr list)))))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (reverse list return)
  (if (null? list)
      return
      (reverse (cdr list) (cons (car list) return)) ))

;2.21

(define (same-parity x . y)
  (define (get-list isAddible list return)
    (cond
      ((null? list) (reverse return '()))
      ((boolean=? isAddible #t) (get-list #f (cdr list) (cons (car list) return)))
      (else (get-list #t (cdr list) return))))
  (if (even? x)
      (get-list #t y '())
      (get-list #t (cons x y) '())))
      


; list mapping

(define (map1 items func)
  (if (null? (cdr items))
      (func (car items))
  (cons (func (car items)) (map (cdr items) func))))
      
(define (map proc items)
  (if (null? items)
      null
      (cons (proc (car items))
            (map proc (cdr items)))))


;2.21
(define (sq a)
  (* a a))

(define (square-list items)
  (if (null? items)
      null
      (cons (sq (car items))
            (square-list (cdr items)))))


(define (square-list-2 items)
  (map (lambda (x) (sq x)) items )   )



;2.22


;2.23

(define (for-e proc items)
  (if (null? items)
      null
  (cons (proc (car items))
        (for-e proc (cdr items)))))


;2.24
;(define x (cons (list 1 2) (list 1 2)))


(define (count-leaves tree)
  
  (cond
    ((null? tree) 0)
    ((not(pair? tree)) 1)
    (else (+ (count-leaves (car tree))
             (count-leaves (cdr tree))))))


(define l (list 1
                 (list 2
                       (list 3 4))))


;2.25
(define v1 '(1 3 (5 7) 9))
(car (cdr (car (cdr (cdr v1)))))

(define v2 '((7)))
(car (car v2))

(define v3 '(1 (2 (3 (4 (5 (6 7)))))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr v3))))))))))))

;2.26

(define x (list 1 2 3))

(define y (list 4 5 6))

(append x y)
(cons x y)
(list x y)