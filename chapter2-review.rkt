#lang racket


; Let's define cons and car, cdr.
(define (cons-fake x y)
  (lambda (x) (if (= x 1) x
                  y)
    ))

(define (car-fake a)
  (a 1))

(define (cdr-fake a)
  (a 2))

; Answer

(define (cons-f2 x y)
  (lambda (m) (m x y)))

(define (car-f2 p)
  (p (lambda (x y) x)))

(define (cdr-f2 p)
  (p (lambda (x y) y)))