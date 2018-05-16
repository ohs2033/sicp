#lang racket


(define (same-parity list)
  (define (iter in out test?)
    (cond ((null? in) out)
          ((test? (car in))
           (iter (cdr in) (cons (car in) out) test?))
          (else (iter (cdr in) out test?))))
  (iter list '() (if (even? (car list))
                     even?
                     odd?)))



;problem 2.6

(define zero (lambda (f) (lambda (x) x)))