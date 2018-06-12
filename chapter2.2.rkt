#lang sicp
;2.21, 2.22, 2.23, 2.24, 2.25, 2.26


;book append, reverse

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
  (cons (func (car items)) (map (cdr items) func))))
      
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car


                   ))
            (map proc (cdr items)))))


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

(define (fringe l)
  (cond ((null? l) l)
        ((list? (car l)) (append (fringe (car l)) (fringe (cdr l))))
        (else (cons (car l) (fringe (cdr l))))))

(define (L f)
  (f c))
  
(newline)
(newline)
(display (fringe randomTree)) ;1 2 3 4
(newline)
(display base1)




  