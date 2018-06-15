#lang sicp
;2.21, 2.22, 2.23, 2.24, 2.25, 2.26


;book append, reverse

;list-ref is 'get' function of list 
(define (list-ref list index)
  (if (= index 0)
      (car list)
      (list-ref (cdr list) (- index 1))))

(define (append2 list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append2 (cdr list1) list2))
      ))

      

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

;2.21 same-parity

(define (same-parity x . y)
  (define (get-list isAddible list return)
    (cond
      ((null? list) (reverse return '()))
      ((boolean? isAddible #t) (get-list #f (cdr list) (cons (car list) return)))
      (else (get-list #t (cdr list) return))))
  (if (even? x)
      (get-list #t y '())
      (get-list #t (cons x y) '())))
      


; list mapping

(define (map1 items func)
  (if (null? (cdr items))
      (func (car items))
  (cons (func (car items)) (map1 (cdr items) func))))
      
(define (map2 proc items)
  (if (null? items)
      nil
      (cons (proc (car


                   ))
            (map2 proc (cdr items)))))


;2.21
(define (sq a)
  (* a a))

(define (square-list items)
  (if (null? items)
      nil
      (cons (sq (car items))
            (square-list (cdr items)))))


(define (square-list-2 items)
  (map (lambda (x) (sq x)) items ))



;2.22


;2.23

(define (for-e proc items)
  (if (null? items)
      nil
  (cons (proc (car items))
        (for-e proc (cdr items)))))

(define (for-eac proc items)
  (define (dummy items)
    (if (null? items) nil
        (cdr (cons (proc (car items)) (dummy (cdr items))))
        ))
  (null? (dummy items)))

(define (for-each-2 proc items)
  (define (dummy input)
    2)
  (cond
    ((null? items) #t)
    ((= 1 (dummy (proc (car items))))
         nil)
    (else (for-each-2 proc (cdr items)))))

(define (for-each3 proc items)
   (cond ((null? items) #t)
         (else (proc (car items))
               (for-each proc (cdr items)))))

(define (for-each f l)
  (if (null? l)
      #t
      ((lambda (x) (for-each f (cdr l)))
       (f (car l)))))


    
(define (for-reverse proc items)
  (define (dummy items)
    (if (null? items)
        nil
        (cdr (cons (proc (car items)) (dummy (cdr items))))
        ))
  (null? (dummy items)))


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

(display (cons x y))


(define (foreach proc list)
  (if (null? list)
      #t
      ((lambda (x) (foreach proc (cdr list)))(proc (car list))) ))


;2.27 deep-reverse

(define (rvrs list)
  (define (iter base list)
    (if (null? list)
        base
        (iter (cons (car list) base) (cdr list))))

  (iter '() list))




(define (deep-reverse list base)
  (cond ((null? list) base)
        ((list? (car list))
         (deep-reverse
          (cdr list)
          (cons (deep-reverse (car list) '()) base)))
        (else
         (deep-reverse (cdr list) (cons (car list) base)))))

(define (dr2 list base)
  (cond ((or (null? list) (not (list? list)))
          list)
        (else
         (dr2 (cdr list)
              (cons (dr2 (car list) base) '())))))

(define randomList (list (list 1 2 (list 10 11 12)) (list 3 4)))

(display randomList)
(display (dr2 randomList '()))


 ;2.28

(define randomTree (list (list 1 2) (list 3 4)))

(define base1 '())

(define (fringe1 l)
  (cond ((null? l) l)
        ((list? (car l)) (append (fringe1 (car l)) (fringe1 (cdr l))))
        (else (cons (car l) (fringe1 (cdr l))))))

;(define (L f)
 ; (f c))
  
;(newline)
;(newline)
;(display (fringe1 randomTree)) ;1 2 3 4
;(newline)
;(display base1)


; using recursion with map, we can deal with tree-shaped data very efficiently.
(define (fringe tree)
  (if (null? tree)
      tree
      (append (fringe (car tree)) (fringe (cdr tree)))))


  
;scale-tree


(define (scale-tree tree factor)
  (map (lambda (subtree)
         (if (pair? subtree)
             (scale-tree subtree factor)
             (* factor subtree)))
       tree))


(define (square-tree tree)
  (map (lambda (subtree)
         (if (pair? subtree)
             (square-tree subtree)
             (* subtree subtree)))
       tree))


(define (square x)
  (* x x))

(define (tree-map tree func)
  (map (lambda (subtree)
         (if (pair? subtree)
             (tree-map subtree func)
             (func subtree)))
       tree))

(display
 (tree-map
   (list 1
         (list 2 (list 3 4) 5)
         (list 6 7))
   square))


; 2.2.3 COMMON INTERFACE.

;---- fibonacci---------------------------------
(define (fib n)
   (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
   (cond ((= count 0) b)
         ((even? count)
          (fib-iter a
                    b
                    (+ (* p p) (* q q))     ; compute p'
                    (+ (* 2 p q) (* q q))   ; compute q'
                    (/ count 2)))
         (else (fib-iter (+ (* b q) (* a q) (* a p))
                         (+ (* b p) (* a q))
                         p
                         q
                         (- count 1)))))
;----------------------------------------------------



(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))



; filter

(define (filter predicate seq)
  (cond ((null? seq) nil)
        ((predicate (car seq))
         (cons (car seq) (filter predicate (cdr seq))))
        (else
         (filter predicate (cdr seq)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(newline)
(newline)
(display 'filterExample)
(newline)
(accumulate + 0 (list 1 2 3 4 5))
(accumulate * 1 (list 1 2 3 4 5))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (list-fib-squares n)
  (accumulate cons
              nil
              (map square
                   (map fib
                        (enumerate-interval 0 n)))))


(define (map p sequence)
  (accumulate (lambda (x y) (cons x y))

:ㅈㅂ

