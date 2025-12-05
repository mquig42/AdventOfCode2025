;;;AoC_04.rkt
;;;2025-12-04
;;;Mike Quigley

;;;This one's a bit different from the others.
;;;I'm going to store the input as a set of coords representing paper rolls
#lang racket

;;coord datatype
(define (make-coord row col)
  (cons row col))
(define (get-row coord)
  (car coord))
(define (get-col coord)
  (cdr coord))

;;Input parsing
(define (parse-line row col line acc)
  (cond ((null? line) acc)
        ((eq? #\@ (car line)) (parse-line row (add1 col) (cdr line)
                                          (set-add acc (make-coord row col))))
        (else (parse-line row (add1 col) (cdr line) acc))))

(define (parse-lines row lines acc)
  (if (null? lines) acc
      (parse-lines (add1 row) (cdr lines)
                   (set-union acc (parse-line row 0 (car lines) acc)))))

(define (enumerate-all-neighbours coord)
  (let ((row (get-row coord))
        (col (get-col coord)))
    (list (make-coord (sub1 row) (sub1 col))
          (make-coord (sub1 row) col)
          (make-coord (sub1 row) (add1 col))
          (make-coord row (sub1 col))
          (make-coord row (add1 col))
          (make-coord (add1 row) (sub1 col))
          (make-coord (add1 row) col)
          (make-coord (add1 row) (add1 col)))))

(define (count-neighbours coord grid)
  (foldl + 0
         (map (λ (x) (if (set-member? grid x) 1 0))
              (enumerate-all-neighbours coord))))

(define input
  (parse-lines 0 (map string->list (file->lines "Input04.txt")) (set)))

(display "Part 1: ")
(length (filter (λ (x) (< (count-neighbours x input) 4))
                (set->list input)))