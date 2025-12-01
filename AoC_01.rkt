#lang racket

(define (solve1 pos count lst)
  (if (null? lst) count
      (let* ((op (if (eq? (string-ref (car lst) 0) #\R) + -))
             (dist (string->number (substring (car lst) 1)))
             (newpos (modulo (op pos dist) 100)))
        (solve1 newpos
                (if (eq? newpos 0) (add1 count) count)
                (cdr lst)))))

(define input (file->lines "Input01.txt"))

(display "Part 1: ")
(solve1 50 0 input)
