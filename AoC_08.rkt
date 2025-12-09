;;;AoC_08.rkt
;;;2025-12-08
;;;Mike Quigley

;;;This was an interesting one
;;;Part 1 reused some code from day 5 to generate the list of circuits
;;;Part 2 I solved manually: keep increasing the number of connections
;;;until I get one circuit of size 1000, then look at the last one in the list.
;;;I might come back and automate it later
#lang racket

;Generates a list of all possible pairs of elements in lst
(define (enumerate-all-pairs lst)
  (define (iter lst-a lst-b acc)
    (cond ((null? (cdr lst-a)) acc)
          ((null? lst-b)
           (iter (cdr lst-a) (cdr (cdr lst-a)) acc))
          (else
           (iter lst-a (cdr lst-b) (cons (list (car lst-a) (car lst-b)) acc)))))
  (iter lst (cdr lst) null))

;Calculates the distance between two points
(define (distance pair)
  (sqrt (+ (expt (- (first (first pair)) (first (second pair))) 2)
           (expt (- (second (first pair)) (second (second pair))) 2)
           (expt (- (third (first pair)) (third (second pair))) 2))))

;Finds the n pairs which are closest together
(define (shortest-distances lst n)
  (take (sort lst < #:key distance #:cache-keys? true) n))

;;Part 1 is essentially merging overlapping ranges, like day 5
;;so I'm using a similar approach
;;circuits are sets of points
(define (set-overlap? a b)
  (not (set-empty? (set-intersect a b))))

(define (merge-all lst)
  ;Merges a against every element of lst
  ;Returns a new list containing:
  ;a with every overlapping range merged onto it
  ;followed by all the non-overlapping ranges
  (define (merge-first a lst acc)
    (cond ((null? lst) (cons a acc))
          ((set-overlap? a (car lst))
           (merge-first (set-union a (car lst)) (cdr lst) acc))
          (else (merge-first a (cdr lst) (cons (car lst) acc)))))
  
  ;Runs merge-first repeatedly to merge entire list
  (define (iter merged unmerged)
    (if (null? unmerged) merged
        (let ((m (merge-first (car unmerged) (cdr unmerged) null)))
          (iter (cons (car m) merged) (cdr m)))))

  ;Before starting, sort lst by the beginning of each range
  (iter null lst))

;Keep running merge-all until it has merged everything it can
(define (really-merge-all lst)
  (let ((merged (merge-all lst)))
    (if (= (length lst) (length merged)) merged
        (really-merge-all merged))))

;Given a list of circuits, return the part 1 solution
(define (solve1 lst)
  (foldl * 1 (take (sort (map set-count lst) >) 3)))

(define input
  (map (λ (x) (map string->number (string-split x ",")))
       (file->lines "Input08.txt")))

(define sets
  (map list->set (shortest-distances (enumerate-all-pairs input) 1000)))

(define merged (really-merge-all sets))

(display "Part 1: ")
(solve1 merged)
(displayln "Part 2: ")
(display "Number of Circuits: ")
(length merged)
(display "Size of First Circuit: ")
(set-count (car merged))
