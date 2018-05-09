#lang racket

(define (pascal m n)
 (cond
   ((< m n) "error. m should not be smaller than n")
   ((or (= m 0) (= n 0) (= m n)) 1 )
       (else (+ (pascal (- m 1) n) (pascal (- m 1) (- n 1))))
       
       ))

;1.17
(define (sm a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1))  ;this is 
                )))
(define (double a)
  (+ a a)
   )

(define (halve a) 
  (cond ((even? a) (/ a 2))
        (else "error")))


            
(define (fastMultiply a b)
  (cond
    ((= b 1) a)
    ((even? b)(double (fastMultiply a (/ b 2)) ))
    (else (+ a (double (fastMultiply a (- b 1))))
     ))) 