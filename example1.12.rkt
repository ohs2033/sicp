#lang racket


(define (pascal m n)
 (cond ((or (= m 0) (= n 0)) 1 )
       (else (+ (pascal (- m 1) n) (pascal (- m 1) (- n 1))))
                   ))