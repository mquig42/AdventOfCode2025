;;;AoC_00.rkt
;;;2025-11-30
;;;Mike Quigley

;;;Simple test program to prepare for tomorrow
#lang racket

(define (read-input filename)
  (map string->number (file->lines filename)))

(define input (read-input "Input00.txt"))

;;Add all numbers from file
(display "Part 1: ")
(foldl + 0 input)

;;Multiply all numbers from file
(display "Part 2: ")
(foldl * 1 input)
