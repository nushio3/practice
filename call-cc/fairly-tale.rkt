#!/usr/bin/env racket
#lang typed/racket

(: x Integer)
(define x 42)



(define exmid (call/cc (lambda (k) (lambda (g) (g '( 'right (lambda (x) (k '('left x)))))))))

(printf "the answer is ~v\n" x)
