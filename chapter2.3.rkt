#lang sicp


(define (equal list1 list2)
  (if (not (pair? list))
      (eq? list1 list2)
  (and (equal (car list1) (car list2))
       (equal (cdr list1) (cdr list2)))))

(display (cadr '((x1 x2) (y1 y2))))


(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))


(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (interaction-set set1 set)
  (cond ((or (null? set) (null? set2))
         '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (interaction-set (cdr set1)
                                set2)))
        (else (interaction-set (cdr set1))
              
                              
         