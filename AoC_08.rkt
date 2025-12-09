;;;AoC_08.rkt
;;;2025-12-08
;;;Mike Quigley
#lang racket

;Generates a list of all possible pairs of elements in lst
(define (enumerate-all-pairs lst)
  (define (iter lst-a lst-b acc)
    (cond ((null? (cdr lst-a)) acc)
          ((null? lst-b)
           (iter (cdr lst-a) (cdr (cdr lst-a)) acc))
          (else
           (iter lst-a (cdr lst-b) (cons (list (car lst-a) (car lst-b)) acc)))))
  (iter lst (cdr lst) null))

;Calculates the distance between two points
(define (distance pair)
  (sqrt (+ (expt (- (first (first pair)) (first (second pair))) 2)
           (expt (- (second (first pair)) (second (second pair))) 2)
           (expt (- (third (first pair)) (third (second pair))) 2))))

;Finds the n pairs which are closest together
(define (shortest-distances lst n)
  (take (sort lst (λ (a b) (< (distance a) (distance b)))) n))

;(define input (map process-line (file->lines "Test08.txt")))
(define input
  (map (λ (x) (map string->number (string-split x ",")))
       (file->lines "Test08.txt")))

(shortest-distances (enumerate-all-pairs input) 10)
