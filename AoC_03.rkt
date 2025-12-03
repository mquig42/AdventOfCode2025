#lang racket

(define (parse-input line)
  (map (λ (x) (- (char->integer x) (char->integer #\0)))
       (string->list line)))

;For part 1, find the largest number in the bank (if it's not last)
;and the largest number that occurs after it

;Returns a sublist starting with the largest value that isn't last
(define (max-not-last p lst)
  (if (null? (cdr lst)) p
      (max-not-last (if (> (car lst) (car p)) lst p) (cdr lst))))

(define (max-joltage bank)
  (let ((p (max-not-last bank bank)))
    (+ (* 10 (car p)) (argmax identity (cdr p)))))

(define input (map parse-input (file->lines "Input03.txt")))

(display "Part 1: ")
(foldl + 0 (map max-joltage input))