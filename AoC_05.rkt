;;;AoC_05.rkt
;;;2025-12-05
;;;Mike Quigley

;;;Part 1 was straightforward
;;;For part 2, we have to merge a list of ranges to find the total
;;;size without double-counting any overlapping regions.
;;;I feel like I've done this before, but a quick search through previous years
;;;didn't turn up anything. Came up with a new solution.
#lang racket

(define (read-ranges file)
  (let ((line (string-trim (read-line file))))
    (if (string=? line "") null
        (cons (map string->number (string-split line "-"))
              (read-ranges file)))))

(define (read-items file)
  (let ((line (read-line file)))
    (if (eof-object? line) null
        (cons (string->number (string-trim line)) (read-items file)))))

(define (fresh? item ranges)
  (cond ((null? ranges) #f)
        ((and (>= item (caar ranges)) (<= item (cadar ranges))) #t)
        (else (fresh? item (cdr ranges)))))

;Do ranges a and b overlap?
(define (overlap? a b)
  (>= (min (cadr a) (cadr b)) (max (car a) (car b))))

;Given two overlapping ranges, return a single combined range
(define (merge a b)
  (list (min (car a) (car b)) (max (cadr a) (cadr b))))

;Merge all ranges in lst, return new list of ranges
(define (merge-all lst)
  ;Merges a against every element of lst
  ;Returns a new list containing:
  ;a with every overlapping range merged onto it
  ;followed by all the non-overlapping ranges
  (define (merge-first a lst acc)
    (cond ((null? lst) (cons a acc))
          ((overlap? a (car lst))
           (merge-first (merge a (car lst)) (cdr lst) acc))
          (else (merge-first a (cdr lst) (cons (car lst) acc)))))
  
  ;Runs merge-first repeatedly to merge entire list
  (define (iter merged unmerged)
    (if (null? unmerged) merged
        (let ((m (merge-first (car unmerged) (cdr unmerged) null)))
          (iter (cons (car m) merged) (cdr m)))))

  ;Before starting, sort lst by the beginning of each range
  (iter null (sort lst (λ (a b) (< (car a) (car b))))))

(define (range-size r)
  (+ 1 (- (second r) (first r))))

(define input-file (open-input-file "Input05.txt"))
(define ranges (read-ranges input-file))
(define items (read-items input-file))
(close-input-port input-file)

(display "Part 1: ")
(length (filter (λ (x) (fresh? x ranges)) items))
(display "Part 2: ")
(foldl + 0 (map range-size (merge-all ranges)))
