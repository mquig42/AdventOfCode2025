;;;AoC_11.rkt
;;;2025-12-11
;;;Mike Quigley
#lang racket

;Read input into a hash map
(define (read-input lines acc)
  (if (null? lines) acc
      (read-input (cdr lines)
                  (hash-set acc
                            (substring (car lines) 0 3)
                            (drop (string-split (car lines) " ") 1)))))

(define (merge-lists a b)
  (if (null? a) b
      (merge-lists (cdr a) (cons (car a) b))))

(define (count-paths stack acc)
  (cond ((null? stack) acc)
        ((string=? (car stack) "out")
         (count-paths (cdr stack) (add1 acc)))
        (else (count-paths
               (merge-lists (hash-ref input (car stack)) (cdr stack))
               acc))))

(define input (read-input (file->lines "Input11.txt") (hash)))

(display "Part 1: ")
(count-paths '("you") 0)
