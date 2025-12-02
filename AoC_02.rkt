;;;AoC_02.rkt
;;;2025-12-02
;;;Mike Quigley

;;;I guess since there are only 12 days this year, we get into tricky math
;;;right away. So far I've gotten part 1, though it's mostly brute force.
#lang racket

;Is x an invalid ID, based on the rules for part 1?
;We can reject anything with an odd number of digits
(define (invalid-id? x)
  (let* ((num-digits (inexact->exact (+ 1 (floor (log x 10)))))
         (pivot (expt 10 (/ num-digits 2))))
    (if (odd? num-digits) #f
        (eq? (modulo x pivot) (quotient x pivot)))))

;Find the sum of all invalid ids in range
(define (process-range lst)
  (foldl + 0 (filter invalid-id? (inclusive-range (first lst) (second lst)))))

(define input (map (λ (x) (map string->number (string-split x "-")))
                   (string-split (file->string "Input02.txt") ",")))

(display "Part 1: ")
(foldl + 0 (map process-range input))
