#lang racket

(define (solve1 pos count lst)
  (if (null? lst) count
      (let* ((op (if (eq? (string-ref (car lst) 0) #\R) + -))
             (dist (string->number (substring (car lst) 1)))
             (newpos (modulo (op pos dist) 100)))
        (solve1 newpos
                (if (eq? newpos 0) (add1 count) count)
                (cdr lst)))))

(define (solve2 pos count lst)
  (if (null? lst) count
      (let* ((op (if (eq? (string-ref (car lst) 0) #\R) + -))
             (dist (string->number (substring (car lst) 1)))
             (newpos (modulo (op pos dist) 100)))
        (solve2 newpos
                (+ (count-zero op pos dist 0) count)
                (cdr lst)))))

(define (count-zero op pos dist count)
  (if (eq? dist 0) count
      (let ((newpos (modulo (op pos 1) 100)))
        (count-zero op newpos (sub1 dist)
                    (if (zero? newpos) (add1 count) count)))))
                  

(define input (file->lines "Input01.txt"))

(display "Part 1: ")
(solve1 50 0 input)
(display "Part 2: ")
(solve2 50 0 input)
