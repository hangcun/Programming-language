
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
(define (sequence low high stride) 
  (if (<= low high)
      (cons low (sequence (+ low stride) high stride))
      null))

(define (string-append-map xs suffix)
  (map (lambda (str) (string-append str suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

(define (stream-for-n-steps s n)
  (if (> n 0)
      (cons (car(s)) (stream-for-n-steps (cdr(s)) (- n 1)))
      null))
                     
(define funny-number-stream 
  (letrec ([f (lambda (x) (cons (if (= 0 (remainder x 5)) (- 0 x) x) 
                                (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define dan-then-dog
  (letrec ([f (lambda (x) (cons x (lambda () (f (if (string=? x "dan.jpg") "dog.jpg" "dan.jpg")))))])
    (lambda () (f "dan.jpg"))))

(define (stream-add-zero s)
  (letrec ([f (lambda (x) (cons (cons 0 (car(x))) (lambda () (f (cdr(x))))))])
    (lambda () (f s))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (n) (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

(define (vector-assoc v vec)
  (letrec ([f (lambda (pos) (cond [(equal? (vector-length vec) pos) #f]
                                  [(and (pair? (vector-ref vec pos)) (equal? (car(vector-ref vec pos)) v)) (vector-ref vec pos)]
                                  [#t (f (+ pos 1))]))])
    (f 0)))

(define (cached-assoc xs n)
  (letrec ([cached-vec (make-vector n #f)]
           [pos-mut 0]
           [f (lambda (v)
                (let ([ans (vector-assoc v cached-vec)])
                  (if ans
                      ans
                      (let ([new-ans (assoc v xs)])
                        (begin 
                          (when new-ans
                              (vector-set! cached-vec pos-mut new-ans)
                              (set! pos-mut (remainder (add1 pos-mut) n)))
                          new-ans)))))])
  f))

(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([v1 e1]
              [f (lambda () (let ([v2 e2])
                              (if (< v2 v1)
                                  (lambda () (f))
                                  #t)))])
         (lambda () (f)))]))

(define (cycle-lists-challenge xs ys)
  (letrec ([f (lambda (xstr ystr) 
                (let ([xhd (car xstr)]
                      [xtail (cdr xstr)]
                      [yhd (car ystr)]
                      [ytail (cdr ystr)])
                  (cons (cons xhd yhd) (lambda () (f (append xtail (list xhd)) (append ytail (list yhd)))))))])
    (lambda () (f xs ys))))

(define (cached-assoc-lru xs n)
  (letrec ([cached-vec (make-vector n #f)]
           [pos-mut 0]
           [lru-pos-vec (build-vector n values)]
           [is-cache-full #f]
           [f (lambda (v)
                (let* ([ans (vector-assoc v cached-vec)]
                      [index (vector-member ans cached-vec)])
                  (if ans
                      (begin
                        (when is-cache-full
                            ((vector-copy! lru-pos-vec 
                                          0 
                                          (vector-append
                                           (vector-filter-not 
                                            (lambda (x) (= x index)) 
                                            lru-pos-vec)
                                           (vector index)) 
                                          0 n))
                            ans))
                      (let ([new-ans (assoc v xs)])
                        (begin 
                          (when new-ans
                             (if is-cache-full
                                (let ([lru-index (vector-ref lru-pos-vec 0)])
                                 (begin
                                  (vector-set! cached-vec lru-index new-ans)
                                  ((vector-copy! lru-pos-vec 
                                                 0 
                                                 (vector-append
                                                  (vector-take-right lru-pos-vec (- n 1))
                                                  (vector lru-index))
                                                 0 n))))
                                (begin
                                  (vector-set! cached-vec pos-mut new-ans)
                                  (set! pos-mut (remainder (add1 pos-mut) n)))))
                          new-ans)))))])
  f))

