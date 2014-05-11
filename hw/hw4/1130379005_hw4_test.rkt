#lang racket

(require "1130379005_hw4.rkt") 

;; A simple library for displaying a 2x3 grid of pictures: used
;; for fun in the tests below (look for "Tests Start Here").

(require (lib "graphics.rkt" "graphics"))

(open-graphics)

(define window-name "Programming Languages, Homework 4")
(define window-width 700)
(define window-height 500)
(define border-size 100)

(define approx-pic-width 200)
(define approx-pic-height 200)
(define pic-grid-width 3)
(define pic-grid-height 2)

(define (open-window)
  (open-viewport window-name window-width window-height))

(define (grid-posn-to-posn grid-posn)
  (when (>= grid-posn (* pic-grid-height pic-grid-width))
    (error "picture grid does not have that many positions"))
  (let ([row (quotient grid-posn pic-grid-width)]
        [col (remainder grid-posn pic-grid-width)])
    (make-posn (+ border-size (* approx-pic-width col))
               (+ border-size (* approx-pic-height row)))))

(define (place-picture window filename grid-posn)
  (let ([posn (grid-posn-to-posn grid-posn)])
    ((clear-solid-rectangle window) posn approx-pic-width approx-pic-height)
    ((draw-pixmap window) filename posn)))

(define (place-repeatedly window pause stream n)
  (when (> n 0)
    (let* ([next (stream)]
           [filename (cdar next)]
           [grid-posn (caar next)]
           [stream (cdr next)])
      (place-picture window filename grid-posn)
      (sleep pause)
      (place-repeatedly window pause stream (- n 1)))))

;; Tests Start Here

; These definitions will work only after you do some of the problems
; so you need to comment them out until you are ready.
; Add more tests as appropriate, of course.

(define nums (sequence 0 5 1))

(define files (string-append-map 
               (list "dan" "dog" "curry" "dog2") 
               ".jpg"))

(define funny-test (stream-for-n-steps funny-number-stream 16))

; a zero-argument function: call (one-visual-test) to open the graphics window, etc.
(define (one-visual-test)
  (place-repeatedly (open-window) 0.5 (cycle-lists nums files) 27))

(define (one-visual-test-challenge)
  (place-repeatedly (open-window) 0.5 (cycle-lists-challenge nums files) 27))

; similar to previous but uses only two files and one position on the grid
(define (visual-zero-only)
  (place-repeatedly (open-window) 0.5 (stream-add-zero dan-then-dog) 20))

(define (visual-only)
  (place-repeatedly (open-window) 0.5 (cycle-lists nums (list "dan.jpg" "dog.jpg")) 27))

(sequence 3 11 2)
(sequence 3 8 3)
(sequence 3 2 1)
(string-append-map '("a" "v" "c") "ss")
(stream-for-n-steps funny-number-stream 10)
(define xs (list 1 2 3))
(define ys (list "a" "b"))
(stream-for-n-steps dan-then-dog 10)
(stream-for-n-steps (stream-add-zero dan-then-dog) 10)
(stream-for-n-steps (cycle-lists nums (list "dan.jpg" "dog.jpg")) 10)
(stream-for-n-steps (cycle-lists xs ys) 10)
(stream-for-n-steps (cycle-lists-challenge xs ys) 10)
(define vec1 (vector (cons 1 1) (cons 2 2) (cons "a" "b")))
(define vec2 (vector 1 2 3 4 5))
(vector-assoc 1 vec1)
(vector-assoc 2 vec1)
(vector-assoc "a" vec1)
(vector-assoc 3 vec1)
(vector-assoc 1 vec2)
(define lst (list (cons 1 1) (cons 2 2) (cons "a" "v")))
((cached-assoc lst 3) 1)
((cached-assoc lst 3) 1)
(define a 2)
(while-less 7 do (begin (set! a (+ a 1)) (print "x") a))
(while-less 7 do (begin (set! a (+ a 1)) (print "x") a))
