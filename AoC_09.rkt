;;;AoC_09.rkt
;;;2025-12-09
;;;Mike Quigley

;;;Part 1 was easy. Part 2 though...
;;;The largest rectangle still has to have two red corners, meaning it's one of
;;;the rectangles enumerated in part 1. The area calculation is the same.
;;;I just need to disqualify anything that's out of bounds.
;;;It's out of bounds if any line segment intersects it in any way.
;;;That would be: first point inside rectangle, second point inside rectangle,
;;;or both points outside it but on opposite sides.
;;;That isn't fully general. It's possible for a rectangle to be completely
;;;out of bounds and not intersect with anything
;;;For example, 2,5->9,7 in the sample input. Let's try it and see if I'm lucky.
;;;Update: I was lucky. This might not work in every case, and it took 23
;;;seconds to run, but it got the right answer.
#lang racket

(define get-x first)
(define get-y second)

;Generates a list of all possible pairs of elements in lst
(define (enumerate-all-pairs lst)
  (define (iter lst-a lst-b acc)
    (cond ((null? (cdr lst-a)) acc)
          ((null? lst-b)
           (iter (cdr lst-a) (cdr (cdr lst-a)) acc))
          (else
           (iter lst-a (cdr lst-b) (cons (list (car lst-a) (car lst-b)) acc)))))
  (iter lst (cdr lst) null))

(define (area rectangle)
  (* (add1 (abs (- (get-x (first rectangle)) (get-x (second rectangle)))))
     (add1 (abs (- (get-y (first rectangle)) (get-y (second rectangle)))))))

;Both rectangle and line are a list of two points
(define (intersects? rectangle line)
  ;Bounds of rectangle
  (let ((x-min (min (get-x (first rectangle)) (get-x (second rectangle))))
        (x-max (max (get-x (first rectangle)) (get-x (second rectangle))))
        (y-min (min (get-y (first rectangle)) (get-y (second rectangle))))
        (y-max (max (get-y (first rectangle)) (get-y (second rectangle)))))
           ;First point of line is inside rectangle
    (cond ((and (> (get-x (first line)) x-min)
                (< (get-x (first line)) x-max)
                (> (get-y (first line)) y-min)
                (< (get-y (first line)) y-max))
           true)
          ;Second point of line is inside rectangle
          ((and (> (get-x (second line)) x-min)
                (< (get-x (second line)) x-max)
                (> (get-y (second line)) y-min)
                (< (get-y (second line)) y-max))
           true)
          ;Vertical line that crosses rectangle
          ((and (= (get-x (first line)) (get-x (second line)))
                (> (get-x (first line)) x-min)
                (< (get-x (first line)) x-max)
                (<= (min (get-y (first line)) (get-y (second line))) y-min)
                (>= (max (get-y (first line)) (get-y (second line))) y-max))
           true)
          ;Horizontal line that crosses rectangle
          ((and (= (get-y (first line)) (get-y (second line)))
                (> (get-y (first line)) y-min)
                (< (get-y (first line)) y-max)
                (<= (min (get-x (first line)) (get-x (second line))) x-min)
                (>= (max (get-x (first line)) (get-x (second line))) x-max))
           true)
          (else false))))

(define (valid? rectangle points)
  (define (iter pts)
    (cond ((null? (cdr pts))
           (not (intersects? rectangle (list (first points) (first pts)))))
          ((intersects? rectangle (list (first pts) (second pts)))
           false)
          (else (iter (cdr pts)))))
  (iter points))

(define (area-2 rectangle)
  (if (valid? rectangle input) (area rectangle) 0))

(define input (map (λ (x) (map string->number (string-split x ",")))
                   (file->lines "Input09.txt")))

(display "Part 1: ")
(argmax identity (map area (enumerate-all-pairs input)))
(display "Part 2: ")
(argmax identity (map area-2 (enumerate-all-pairs input)))
