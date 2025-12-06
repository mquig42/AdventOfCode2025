#lang racket

(define (process-line line)
  (if (char-numeric? (string-ref (string-trim line) 0))
      (map string->number (string-split line))
      (map (λ (x) (read (open-input-string x))) (string-split line))))

(define (solve1 operators numbers acc)
  (cond ((null? operators) acc)
        ((eq? (car operators) '+)
         (solve1 (cdr operators) (map cdr numbers)
                 (+ acc (foldl + 0 (map car numbers)))))
        (else
         (solve1 (cdr operators) (map cdr numbers)
                 (+ acc (foldl * 1 (map car numbers)))))))

(define input (map process-line (file->lines "Input06.txt")))
(define operators (last input))
(define numbers (drop-right input 1))

(display "Part 1: ")
(solve1 operators numbers 0)