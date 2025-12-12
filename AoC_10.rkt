;;;AoC_10.rkt
;;;2025-12-10
;;;Mike Quigley

;;;Part 1: Find the smallest number of moves to reach an end state, eh?
;;;This looks like a job for bread first search.
;;;It seems likely that part 2 will be a similar question, but using the last
;;;section of each line to weight the moves. Dijkstra could solve that.
;;;This is the year I finally make a fully-general Dijkstra solver
#lang racket
