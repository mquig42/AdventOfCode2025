;;;AoC_02.rkt
;;;2025-12-02
;;;Mike Quigley

;;;I guess since there are only 12 days this year, we get into tricky math
;;;Generate sets of every possible invalid ID and use set-intersect
;;;Within the range of my input, invalid IDs must have between 2 and 10 digits
;;;For part 1, any number from 1-99999 can be repeated twice
;;;For part 2, single digit numbers can be repeated up to 11 times,
;;;2 digit numbers 5 times
;;;3 digit numbers 3 times
;;;4 or 5 digit numbers twice
;;;Tried that approach for part 1. It works, but is a bit slower
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

(define (process-range-2 lst)
  (foldl + 0
         (set->list
          (set-intersect all-invalids-2
           (list->set (inclusive-range (first lst) (second lst)))))))

;Generates a repeated number
;eg. (repeat 123 2) returns 123123
;Need to use that cond block because (log 1000 10) is slightly off
(define (repeat n times)
  (define (iter times acc multiplier)
    (if (eq? times 1) acc
        (iter (- times 1) (+ (* acc multiplier) n) multiplier)))
  (iter times n (cond ((> n 9999) 100000)
                      ((> n 999) 10000)
                      ((> n 99) 1000)
                      ((> n 9) 100)
                      (else 10))))

(define (repeat-all n)
  (define (iter times acc)
    (if (eq? times 1) acc
        (iter (- times 1) (set-add acc (repeat n times)))))
  (let ((times (cond ((> n 999) 2)
                     ((> n 99) 3)
                     ((> n 9) 5)
                     (else 10))))
    (iter times (set))))

(define all-invalids-2
  (foldl set-union (set) (map repeat-all (range 1 100000))))

(define input (map (λ (x) (map string->number (string-split x "-")))
                   (string-split (file->string "Input02.txt") ",")))

(display "Part 1: ")
(foldl + 0 (map process-range input))
(display "Part 2: ")
(foldl + 0 (map process-range-2 input))
