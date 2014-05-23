#lang racket

(require "hw5.rkt")

; a test case that uses problems 1, 2, and 4
; should produce (list (int 10) (int 11) (int 16))

(define test1
  (mupllist->racketlist
   (eval-exp (call (call mupl-mapAddN (int 7))
                   (racketlist->mupllist 
                    (list (int 3) (int 4) (int 9)))))))

(define al (cons (int 1) (cons (int 2) (cons (var "hi") (aunit)))))

(define pa (apair (int 1) al))

(mupllist->racketlist (apair (apair (int 1) (int 2)) (apair (var "hi") (aunit))))
(eval-exp (int 1))
(define a1 (eval-exp (add (int 1) (int 2))))
(define test-ifgreater (eval-exp (ifgreater a1 (int 1) (add (int 10) (add (int 2) (int 3))) (int 0))))
(define test-fun (fun #f "arg" (add (var "arg") (var "arg"))))
(define test-env (cons (cons "a" (int 1)) (cons (cons "b" (int 2)) (cons (cons "arg" (int 10)) null))))
(define test-call (call (closure test-env test-fun) (int 30)))
(define test-call2 (call (closure (list (cons (var "a") (int 1)) (cons (var "b") (int 2)) (cons (var "arg") (int 10))) (fun #f (var "arg") (add "arg" "arg"))) (int 20)))

(equal? (list (int 3) (int 5) (var "hi"))
        (mupllist->racketlist (apair (int 3) (apair (int 5) (apair (var "hi") (aunit))))))
    
(equal? (apair (var "hi") (apair (int 666) (apair (var "test") (aunit))))
        (racketlist->mupllist (list (var "hi") (int 666) (var "test"))))
    
(equal?
 (int 2)
 (eval-exp (mlet "f1"
                 (fun "f1" "a" (mlet "x" (var "a") (fun "f2" "z" (add (var "x") (int 1)))))
                 (mlet "f3" (fun "f3" "f" (mlet "x" (int 1729) (call (var "f") (aunit))))
                       (call (var "f3") (call (var "f1") (int 1)))))))
    
(equal?
 (int 43)
 (eval-exp (call (fun "incr" "x" (add (var "x") (int 1))) (int 42))))  
  
(equal?
 (int 0)
 (eval-exp (ifgreater (add (int 2) (int 2)) (add (int 2) (int 1)) (add (int 3) (int -3)) (add "wrong" "bad"))))

(equal?
 (apair (int 1) (int 4))
 (eval-exp (apair (fst (apair (int 1) (int 2)))
                  (snd (apair (int 3) (int 4))) )))
    
(equal?
 (int 6)
 (eval-exp (mlet "fnc"
                 (fun "f1" "x"
                      (ifgreater (isaunit (var "x")) (int 0)
                                 (int 0)
                                 (add (fst (var "x")) (call (var "f1") (snd (var "x"))))))
                 (call (var "fnc") (apair (int 1) (apair (int 2) (apair (int 3) (aunit))))))))

(equal?
 (int 2)
 (eval-exp (ifaunit (aunit) (int 2) (int 3))))


(equal? (int 1)
        (eval-exp (mlet* (cons (cons "x" (int 1)) null) (var "x"))))

(equal? (int 20)
        (eval-exp (mlet* (list (cons "f" (int 2)) (cons "y" (int 15))) (add (var "f") (add (var "y") (int 3))))))


(equal? (int 1)
        (eval-exp (ifeq (int 2) (add (int 1) (int 1)) (int 1) (int 2))))
           
(equal? (int 2)
        (eval-exp (ifeq (int 2) (add (int 1) (int 2)) (int 1) (int 2))))
    
(define addtwo (fun "addone" "x" (add (var "x") (int 2))))
(define mupl-map-addtwo (call mupl-map addtwo))
(equal? (eval-exp (call mupl-map-addtwo (aunit))) (aunit))

(define my-mupl-list (apair (int 23) (apair (int 42) (aunit))))
(define my-answers (apair (int 25) (apair (int 44) (aunit))))
(equal? (eval-exp (call mupl-map-addtwo my-mupl-list)) my-answers)
    
;test-case "mupl-mapAddN"
(define input (apair (int 25) (apair (int 44) (aunit))))
(define output (apair (int 26) (apair (int 45) (aunit))))
(equal? (eval-exp (call (call mupl-mapAddN (int 1)) input)) output)

(equal? (eval-exp (call (call mupl-mapAddN (int 7))
                              (racketlist->mupllist '())))
              (aunit))
(equal? (eval-exp (call (call mupl-mapAddN (int 7))
                              (racketlist->mupllist (list (int 3) (int 4) (int 9)))))
              (racketlist->mupllist (list (int 10) (int 11) (int 16))))
(equal? (eval-exp (call (call mupl-mapAddN (int 7))
                              (racketlist->mupllist (list (int 3)))))
              (racketlist->mupllist (list (int 10))))
    

