#lang pl untyped

(define x 123)
(define (getx) x)
(define (bar1 x) (getx))
(define (bar2 y) (getx))

(define (foo x)
  (define (helper) (+ x 1)) helper
  )

(test ((foo 0)) => 1)

(define (add x y) (+ x y))

(test (let ([+ *]) (add 6 7)) => 13 )

(test (getx) => 123)
(test (let ([x 456]) (getx)) => 123 )

(test (getx) => 123)
(test (bar1 999) => 123)
(test (bar2 999) => 123)