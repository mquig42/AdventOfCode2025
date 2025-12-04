;;;AoC_03.rkt
;;;2025-12-03
;;;Mike Quigley

;;;This is the first one I needed help with.
;;;I made a recursive function that produces correct results,
;;;but even with optimization and memoization it's too slow for a bank of 100
;;;This is why you don't make 2 recursive calls from the same function
;;;Fibonacci has the same problem, but memoization fixes it.
;;;Anyway, everyone on the subreddit is using stacks, so that's how I got part 2
#lang racket

(define (parse-input line)
  (map (λ (x) (- (char->integer x) (char->integer #\0)))
       (string->list line)))

;This gets the right answer for the test input and part 1
;but is too slow for part 2
(define (max-joltage-recursive batteries bank)
  (define (all-remaining acc bank)
    (if (null? bank) acc
        (all-remaining (+ (* acc 10) (car bank)) (cdr bank))))
  (define (iter acc batteries bank-remaining bank)
    (cond ((null? bank) acc)
          ((zero? batteries) acc)
          ((eq? batteries bank-remaining) (all-remaining acc bank))
          (else
           (max (iter (+ (* acc 10) (car bank))
                      (sub1 batteries) (sub1 bank-remaining) (cdr bank))
                (iter acc batteries (sub1 bank-remaining) (cdr bank))))))
  (iter 0 batteries (length bank) bank))

;Given a stack of digits representing an integer, with the last digit on top,
;return the integer
(define (unstack stack)
  (define (iter acc lst)
    (if (null? lst) acc
        (iter (+ (* acc 10) (car lst)) (cdr lst))))
  (iter 0 (reverse stack)))

(define (max-joltage-stack batteries bank)
  (define (iter batteries bank-remaining bank stack)
    (cond ((null? bank) (unstack stack)) ;Reached end. Return.
          ;Stack has room. Add next battery.
          ((or (null? stack)
               (eq? batteries bank-remaining)
               (and (>= (car stack) (car bank)) (> batteries 0)))
           (iter (sub1 batteries) (sub1 bank-remaining)
                 (cdr bank) (cons (car bank) stack)))
          ;stack contains maximum number of batteries and top is
          ;more than next one in bank. Throw away next battery.
          ((and (zero? batteries) (> (car stack) (car bank)))
           (iter batteries (sub1 bank-remaining) (cdr bank) stack))
          ;Top of stack is smaller than next battery. Throw it away
          (else (iter (add1 batteries) bank-remaining bank (cdr stack)))))
  (iter batteries (length bank) bank '()))
          

(define input (map parse-input (file->lines "Input03.txt")))

(display "Part 1: ")
(foldl + 0 (map (λ (x) (max-joltage-recursive 2 x)) input))
(display "Part 2: ")
(foldl + 0 (map (λ (x) (max-joltage-stack 12 x)) input))