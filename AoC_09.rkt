;;;AoC_09.rkt
;;;2025-12-09
;;;Mike Quigley
#lang racket

(define get-x first)
(define get-y second)

;Generates a list of all possible pairs of elements in lst
(define (enumerate-all-pairs lst)
  (define (iter lst-a lst-b acc)
    (cond ((null? (cdr lst-a)) acc)
          ((null? lst-b)
           (iter (cdr lst-a) (cdr (cdr lst-a)) acc))
          (else
           (iter lst-a (cdr lst-b) (cons (list (car lst-a) (car lst-b)) acc)))))
  (iter lst (cdr lst) null))

(define (area pair)
  (* (add1 (abs (- (get-x (first pair)) (get-x (second pair)))))
     (add1 (abs (- (get-y (first pair)) (get-y (second pair)))))))

(define input (map (λ (x) (map string->number (string-split x ",")))
                   (file->lines "Input09.txt")))

(display "Part 1: ")
(argmax identity (map area (enumerate-all-pairs input)))
