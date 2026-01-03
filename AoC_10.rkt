;;;AoC_10.rkt
;;;2025-12-10
;;;Mike Quigley

;;;Part 1: Find the smallest number of moves to reach an end state, eh?
;;;This looks like a job for bread first search.
;;;It seems likely that part 2 will be a similar question, but using the last
;;;section of each line to weight the moves. Dijkstra could solve that.
;;;This is the year I finally make a fully-general Dijkstra solver
#lang racket
(require data/heap)

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

;Getters
(define (goal machine)
  (first machine))
(define (buttons machine)
  (second machine))
(define (joltages machine)
  (third machine))

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

(define (enumerate-moves machine state)
  (map (λ (x) (cons (add1 (car state)) (bitwise-xor (cdr state) x)))
       (buttons machine)))

;;Queue functions
;Creates a queue element, which has a priority and value
(define (q-element priority value)
  (cons priority value))
;Compare priority of two queue elements
(define (q<= a b)
  (<= (car a) (car b)))
;Changes an element's priority
(define (q-reduce-priority! q value from to)
  (heap-remove-eq! q (q-element from value))
  (heap-add! q (q-element to value)))
;Returns value of element with lowest priority and removes it from queue
(define (q-lowest! q)
  (let ((lowest (heap-min q)))
    (heap-remove-min! q)
    lowest))

(define (dijkstra grid start goal? enumerate-moves)
  (let ((unvisited (make-heap q<=))
        (distances (make-hash)))
    (define (visit node)
      (let ((dist (car node))
            (state (cdr node)))
        (cond ((goal? state) dist)
              (else
               (for-each (λ (x)
                           (let ((old-dist (hash-ref distances (cdr x) +inf.0))
                                 (new-dist (car x)))
                             (cond ((> old-dist new-dist)
                                    (hash-set! distances (cdr x) new-dist)
                                    (q-reduce-priority!
                                     unvisited (cdr x) old-dist new-dist)))))
                         (filter (λ (x) (not (hash-has-key? distances (cdr x))))
                                 (enumerate-moves grid node)))
               (hash-set! distances state dist)
               (visit (q-lowest! unvisited))))))
    (heap-add! unvisited (q-element 0 start))
    (hash-set! distances start 0)
    (visit (q-lowest! unvisited))))

(define input (map parse-machine (file->lines "Input10.txt")))

(display "Part 1: ")
(foldl + 0
       (map (λ (machine)
              (dijkstra machine 0 (λ (x) (= x (car machine))) enumerate-moves))
            input))