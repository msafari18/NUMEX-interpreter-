#lang racket

(require "project.rkt")

; This file uses Racket's unit-testing framework, which is convenient but not required of you.

; Note we have provided [only] 3 tests, but you can't run them until do some of the assignment.
; You will want more tests.

(require rackunit)

(define tests
  (test-suite
   "Project Tests"

   (check-equal? (eval-exp (plus (num 8) (num 4 ))) (num 12) "plus simple test")
   (check-exn (lambda (x) (string=? (exn-message x) "NUMEX addition applied to non-number"))
              (lambda () (eval-exp (plus (num 2) (bool #t))))
              "plus bad argument")
   (check-equal? (eval-exp (mult (num 8) (num 4 ))) (num 32) "mult simple test")
   (check-exn (lambda (x) (string=? (exn-message x) "NUMEX multiply applied to non-number"))
              (lambda () (eval-exp (mult (bool #t) (num 4 ))))
              "mult bad argument")
   (check-equal? (eval-exp (minus (num 8) (num 4 ))) (num 4) "minus simple test")
   (check-exn (lambda (x) (string=? (exn-message x) "NUMEX minus applied to non-number"))
              (lambda () (eval-exp (minus (bool #t) (num 4 ))))
              "minus bad argument")
   (check-equal? (eval-exp (div (num 8) (num 3 ))) (num 2) "div simple test")
   (check-exn (lambda (x) (string=? (exn-message x) "NUMEX divide applied to non-number"))
              (lambda () (eval-exp (div (bool #t) (num 4 ))))
              "minus bad argument")
   (check-equal? (eval-exp (neg (num 8))) (num -8) "neg simple test")
   (check-equal? (eval-exp (neg (bool #t))) (bool #f) "neg simple test")
   
   (check-equal? (eval-exp (andalso (bool #f) (num 1 ))) (bool #f))
   (check-equal? (eval-exp (andalso (bool #t) (bool #t ))) (bool #t))
   (check-equal? (eval-exp (orelse (bool #f) (bool #f ))) (bool #f))
   (check-equal? (eval-exp (orelse (bool #t) (num 2 ))) (bool #t))

   
   (check-equal? (eval-exp (iseq (num 4) (num 3 ))) (bool #f ))
   (check-equal? (eval-exp (iseq (bool #t) (bool #t))) (bool #t ))
   (check-equal? (eval-exp (iseq (num 4) (bool #t ))) (bool #f ))

   (check-equal? (eval-exp (cnd (iseq (num 4) (num 4 )) (minus (num 8) (num 4 ))(num 4))) (num 4 ))
   (check-equal? (eval-exp (ifnzero(minus (num 8) (num 8 )) (minus (num 8) (num 4 ))(mult (num 8) (num 4 )))) (num 32 ))
   (check-equal? (eval-exp (ifnzero(minus (num 8) (num 4 )) (minus (num 8) (num 4 ))(mult (num 8) (num 4 )))) (num 4 ))
   (check-equal? (eval-exp (ifleq(minus (num 8) (num 4 )) (minus (num 8) (num 3 ))(mult (num 8) (num 4 ))(mult (num 8) (num 5 )))) (num 32 ))
   (check-equal? (eval-exp (ifleq(minus (num 8) (num 2 )) (minus (num 8) (num 3 ))(mult (num 8) (num 4 ))(mult (num 8) (num 5 )))) (num 40 ))
   (check-equal? (eval-exp (ifmunit(munit)(num 2)(num 3)))(num 2))
   (check-equal? (eval-exp (ifneq(num 2)(num 2)(num 3)(num 6)))(num 6))

   (check-equal? (numexlist->racketlist
                  (eval-exp (apply (apply numex-all-gt (num 9))
                                  (racketlist->numexlist 
                                   (list (num 10) (num 9) (num 15))))))
                 (list (num 10) (num 15))
                 "provided combined test using problems 1, 2, and 4")
   ))


(require rackunit/text-ui)
;; runs the test
(run-tests tests)
