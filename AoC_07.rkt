;;;AoC_07.rkt
;;;2025-12-07
;;;Mike Quigley

;;;For part 1, we don't have to worry yet about overlapping beams
;;;The question is "how many splitters will be hit by any beam?"
;;;For example, the sample contains 22 splitters, and all but one of them
;;;(the 5th one on the bottom row) are hit, so 21 in total.
;;;Another thing to note: Every splitter is surrounded by empty space,
;;;so the new beams it creates will not go directly onto another splitter.
#lang racket

;;Coord datatype
(define (make-coord row col)
  (cons row col))
(define (get-row coord)
  (car coord))
(define (get-col coord)
  (cdr coord))

;Generate a set of coords for all beam splitters
(define (read-splitters row lines splitters)
  (define (read-line row col line splitters)
    (cond ((null? line) splitters)
          ((eq? #\^ (car line))
           (read-line row (add1 col) (cdr line)
                      (set-add splitters (make-coord row col))))
          (else
           (read-line row (add1 col) (cdr line) splitters))))
  (if (null? lines) splitters
      (read-splitters (add1 row) (cdr lines)
                      (read-line row 0 (string->list (car lines)) splitters))))

(define (count-splits row beams count)
  42)

(define input (file->lines "Test07.txt"))
(define splitters (read-splitters 0 input (set)))
(define beam-start (set (string-find (car input) "S")))
(define rowmax (length input))
(define colmax (string-length (car input)))

(display "Part 1: ")
(count-splits 0 beam-start 0)
