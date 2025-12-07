;;;AoC_07.rkt
;;;2025-12-07
;;;Mike Quigley

;;;For part 1, we don't have to worry yet about overlapping beams
;;;The question is "how many splitters will be hit by any beam?"
;;;For example, the sample contains 22 splitters, and all but one of them
;;;(the 5th one on the bottom row) are hit, so 21 in total.
;;;Another thing to note: Every splitter is surrounded by empty space,
;;;so the new beams it creates will not go directly onto another splitter.
;;;For part 2, they're going with a new interpretation. Instead of a beam
;;;splitter creating two beams, it creates two timelines. I have to find the
;;;total number of timelines created.
;;;First question: why isn't it just the number of splits times 2?
;;;Not all splits are equal. Beams recombine, so there may be multiple ways
;;;of reaching the same place. The first splitter creates two timelines,
;;;a later one (with 5 paths leading to it) creates 10.
#lang racket

;;Coord datatype
(define (make-coord row col)
  (cons row col))
(define (get-row coord)
  (car coord))
(define (get-col coord)
  (cdr coord))

;Generate a set of coords for all beam splitters
(define (read-splitters lines)
  (define (iter row col lines splitters)
    (cond ((= row rowmax) splitters)
          ((= col colmax)
           (iter (add1 row) 0 (cdr lines) splitters))
          ((eq? (string-ref (car lines) col) #\^)
           (iter row (add1 col) lines
                 (set-add splitters (make-coord row col))))
          (else (iter row (add1 col) lines splitters))))
  (iter 0 0 lines (set)))

(define (count-splits)
  (define (iter row beams new-beams count)
    (cond ((= row rowmax) count)
          ((set-empty? beams)
           (iter (add1 row) new-beams (set) count))
          ((set-member? splitters (make-coord row (set-first beams)))
           (iter row (set-rest beams)
                 (set-add (set-add new-beams (sub1 (set-first beams)))
                          (add1 (set-first beams)))
                 (add1 count)))
          (else (iter row (set-rest beams)
                      (set-add new-beams (set-first beams))
                      count))))
  (iter 0 beam-start (set) 0))

(define input (file->lines "Input07.txt"))
(define rowmax (sub1 (length input)))
(define colmax (sub1 (string-length (car input))))
(define splitters (read-splitters input))
(define beam-start (set (string-find (car input) "S")))

(display "Part 1: ")
(count-splits)
