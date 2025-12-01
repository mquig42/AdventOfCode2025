;;;AoC_01.rkt
;;;2025-12-01
;;;Mike Quigley

;;;And we're off!
;;;Refactored my original solution. The solve function is much simpler.
;;;count-zero-2 is still inefficient. It's fast enough, but constant time
;;;should be possible.
#lang racket

(define (parse-input line)
  (cons (if (eq? (string-ref line 0) #\R) + -)
        (string->number (substring line 1))))

(define (solve count-zero pos count lst)
  (if (null? lst) count
      (solve count-zero
             (modulo ((caar lst) pos (cdar lst)) 100)
             (+ count (count-zero (caar lst) pos (cdar lst) 0))
             (cdr lst))))

;Count the number of times the dial ends at zero
(define (count-zero-1 op pos dist count)
  (if (zero? (modulo (op pos dist) 100)) 1 0))

;Count the number of times the dial passes zero
(define (count-zero-2 op pos dist count)
  (if (eq? dist 0) count
      (let ((newpos (modulo (op pos 1) 100)))
        (count-zero-2 op newpos (sub1 dist)
                      (if (zero? newpos) (add1 count) count)))))
                  

(define input (map parse-input (file->lines "Input01.txt")))

(display "Part 1: ")
(solve count-zero-1 50 0 input)
(display "Part 2: ")
(solve count-zero-2 50 0 input)
