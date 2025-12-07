;;;AoC_06.rkt
;;;2025-12-06
;;;Mike Quigley

;;;I thought I could solve parts 1 and 2 the same way by transforming the data
;;;Currently, it didn't work out that way.
;;;This is probably more complicated than it needs to be.
;;;I should redo it later.
#lang racket

(define (parse-numbers str)
  (map string->number (string-split str)))

(define (parse-numbers-2 strs)
  (define (iter strs current acc)
    (cond ((null? strs) (cons current acc))
          ((string=? (string-trim (car strs)) "")
           (iter (cdr strs) null (cons current acc)))
          (else
           (iter (cdr strs)
                 (cons (string->number (string-trim (car strs))) current)
                 acc))))
  (iter strs null null))

(define (total operators numbers acc)
  (cond ((null? operators) acc)
        ((eq? (car operators) '+)
         (total (cdr operators) (map cdr numbers)
                 (+ acc (foldl + 0 (map car numbers)))))
        (else
         (total (cdr operators) (map cdr numbers)
                 (+ acc (foldl * 1 (map car numbers)))))))

(define (total-2 operators numbers acc)
  (cond ((null? operators) acc)
        ((eq? (car operators) '+)
         (total-2 (cdr operators) (cdr numbers)
                  (+ (foldl + 0 (car numbers)) acc)))
        (else
         (total-2 (cdr operators) (cdr numbers)
                  (+ (foldl * 1 (car numbers)) acc)))))

(define (get-col col strs)
  (build-string (length strs)
                (λ (x) (string-ref (list-ref strs x) col))))

(define (transpose-strings strings)
  (map (λ (x) (get-col x strings)) (range (string-length (car strings)))))

(define lines (file->lines "Input06.txt"))
(define number-strs (drop-right lines 1))
(define operators
  (map (λ (x) (read (open-input-string x))) (string-split (last lines))))

(display "Part 1: ")
(total operators (map parse-numbers number-strs) 0)
(display "Part 2: ")
(total-2 operators
         (reverse (parse-numbers-2 (transpose-strings number-strs))) 0)
