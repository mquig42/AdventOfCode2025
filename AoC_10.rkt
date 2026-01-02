;;;AoC_10.rkt
;;;2025-12-10
;;;Mike Quigley

;;;Part 1: Find the smallest number of moves to reach an end state, eh?
;;;This looks like a job for bread first search.
;;;It seems likely that part 2 will be a similar question, but using the last
;;;section of each line to weight the moves. Dijkstra could solve that.
;;;This is the year I finally make a fully-general Dijkstra solver
#lang racket

;;Machine description:
;;1. Goal state
;;2. List of buttons
;;3. Joltage requirements
;;Goal and buttons are stored as integers, so the toggle operation can be
;;bitwise XOR

(define (remove-brackets str)
  (substring str 1 (- (string-length str) 1)))

(define (extract-numbers str)
  (map string->number (string-split (remove-brackets str) ",")))

(define (parse-goal str)
  (define (iter lights bit acc)
    (if (null? lights) acc
        (iter (cdr lights)
              (* 2 bit)
              (if (eq? (car lights) #\#) (+ acc bit) acc))))
  (iter (string->list (remove-brackets str)) 1 0))

(define (parse-button str)
  (foldl (λ (a b) (+ (expt 2 a) b)) 0 (extract-numbers str)))

(define (parse-machine str)
  (let ((sp (string-split str)))
    (list (parse-goal (car sp))
          (map parse-button (drop-right (cdr sp) 1))
          (extract-numbers (last sp)))))

;For debug purposes, convert an integer representation of the panel lights
;into a string, using '.' for off and '#' for on
;Does not include trailing offs
;For example, ".##", ".##.", and ".##..." are all represented by 6
(define (lights->string lights)
  (define (largest-pow2 n acc)
    (if (> acc n) (/ acc 2)
        (largest-pow2 n (* acc 2))))
  (define (iter lights pow2 lst)
    (cond ((= pow2 1/2) lst)
          ((>= lights pow2)
           (iter (- lights pow2) (/ pow2 2) (cons #\# lst)))
          (else
           (iter lights (/ pow2 2) (cons #\. lst)))))
  (list->string (iter lights (largest-pow2 lights 1) null)))

(define input (map parse-machine (file->lines "Test10.txt")))
