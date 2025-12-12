;;;AoC_11.rkt
;;;2025-12-11
;;;Mike Quigley

;;;Memoization can be a powerful thing. So can hash maps.
#lang racket
(require memo)

;Read input into a hash map
(define (read-input lines acc)
  (if (null? lines) acc
      (read-input (cdr lines)
                  (hash-set acc
                            (substring (car lines) 0 3)
                            (drop (string-split (car lines) " ") 1)))))

;Using memoize because the number of paths is very large
;dac and fft are bools which track whether we've passed them on our current path
(define/memoize (count-paths start dac fft)
  (if (and (string=? start "out") dac fft) 1
      (foldl + 0
             (map (λ (x) (count-paths x (or dac (string=? start "dac"))
                                      (or fft (string=? start "fft"))))
                  (hash-ref input start null)))))

(define input (read-input (file->lines "Input11.txt") (hash)))

(display "Part 1: ")
(count-paths "you" true true)
(display "Part 2: ")
(count-paths "svr" false false)
