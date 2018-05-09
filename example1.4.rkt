#lang racket

;problem 2.2

;start-segment, end-segment, make-segment

(define (avg a b)
  (/ (+ a b)))

(define (make-point x y)
  (cons x y))

(define (start-segment line)
  (car line))

(define (end-segment line)
  (cdr line))

(define (midpoint-segment a b)
  (cons (avg (car a) (car b)) (avg (cdr a) (cdr b))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
