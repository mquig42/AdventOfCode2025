;;;AoC_08.rkt
;;;2025-12-08
;;;Mike Quigley
#lang racket

(define (process-line line)
  (map string->number (string-split line ",")))

(define input (map process-line (file->lines "Test08.txt")))
