;; PL Project - Fall 2018
;; NUMEX interpreter

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for NUMEX programs

;; CHANGE add the missing ones

(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct num  (int)    #:transparent)  ;; a constant number, e.g., (num 17)
(struct bool  (boolean)    #:transparent)
(struct plus  (e1 e2)  #:transparent)  ;; add two expressions
(struct minus  (e1 e2)  #:transparent) ;; mines tow expression
(struct mult  (e1 e2)  #:transparent) ;; multiply two expression
(struct div  (e1 e2)  #:transparent) ;; divide two expression
(struct neg  (e1)  #:transparent) ;; negate
(struct andalso  (e1 e2)  #:transparent) ;;
(struct orelse  (e1 e2)  #:transparent) ;;
(struct cnd  (e1 e2 e3)  #:transparent) ;; if else
(struct iseq  (e1 e2)  #:transparent) ;; comparison
(struct ifnzero  (e1 e2 e3)  #:transparent) ;; zero if else
(struct ifleq  (e1 e2 e3 e4)  #:transparent) ;;
(struct lam  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct apply (funexp actual)       #:transparent) ;; function application
(struct with  (s e1 e2)  #:transparent) ;; 
(struct apair  (e1 e2)  #:transparent) ;; pair constructor
(struct 1st  (e1)  #:transparent) ;; first of teh pair
(struct 2nd  (e1)  #:transparent) ;; second of teh pair
(struct munit   ()      #:transparent) ;; unit value -- good for ending a list
(struct ismunit (e)     #:transparent) ;; if e1 is unit then true else false

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env f) #:transparent) 

;; Problem 1

(define (racketlist->numexlist xs)
  (cond [(null? xs) (munit)]
        [#t (apair(car xs)(racketlist->numexlist(cdr xs)))]
  ))

(define (numexlist->racketlist xs)
  (cond [(munit? xs) '()]
        [#t (cons(apair-e1 xs)(numexlist->racketlist (apair-e2 xs)))]
  ))


;; Problem 2

;; lookup a variable in an environment
;; Complete this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
  	[(equal? (car(car env)) str) (cdr(car env))]
        [#t (envlookup (cdr env) str)]
		)
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;?

;; Complete more cases for other kinds of NUMEX expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(plus? e) 
         (let ([v1 (eval-under-env (plus-e1 e) env)]
               [v2 (eval-under-env (plus-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (+ (num-int v1) 
                       (num-int v2)))
               (error "NUMEX addition applied to non-number")))]
        [(num? e)
         (if (integer? (num-int e)) e (error "NUMEX not number in num struct"))]
        
        [(bool? e)
         (if (boolean? (bool-boolean e)) e (error "NUMEX not boolean in bool struct"))]

        [(minus? e)
         (let([v1 (eval-under-env (minus-e1 e) env)]
              [v2 (eval-under-env (minus-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (- (num-int v1)
                         (num-int v2)))
               (error "NUMEX minus applied to non-number")))]
        [(mult? e)
         (let([v1 (eval-under-env (mult-e1 e) env)]
              [v2 (eval-under-env (mult-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (* (num-int v1)
                         (num-int v2)))
               (error "NUMEX multiply applied to non-number")))]
        [(div? e)
         (let([v1 (eval-under-env (div-e1 e) env)]
              [v2 (eval-under-env (div-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (quotient (num-int v1) (num-int v2)))
               (error "NUMEX divide applied to non-number")))]
;;;;; divide by zero should be handled ?
        [(neg? e)
         (let([v1 (eval-under-env (neg-e1 e) env )])
           (if (num? v1)
               (num(-(num-int v1)))
               (if (bool? v1)
               (bool(not(bool-boolean v1)))
               (error "NUMEX not num or boolean"))))]

       
        [(andalso? e)
         (let([v1 (eval-under-env (andalso-e1 e) env)])
           (if (bool? v1)
               (if (equal?(bool-boolean v1) #f)
                   (bool #f)
                   (let([v2 (eval-under-env (andalso-e2 e) env)])
                     (if (bool? v2)
                     (if (equal? (bool-boolean v2) #f)(bool #f)
                         (bool #t))(error "NUMEX not boolean"))))(error "NUMEX not bool")))]

        [(orelse? e)
         (let([v1 (eval-under-env (orelse-e1 e) env)])
           (if (bool? v1)
               (if (equal?(bool-boolean v1) #t)
                   (bool #t)
                   (let([v2 (eval-under-env (orelse-e2 e) env)])
                     (if (bool? v2)
                     (if (equal? (bool-boolean v2) #f)(bool #f)
                         (bool #t))(error "NUMEX not boolean"))))(error "NUMEX not bool")))]
        
     

        [(cnd? e)
         (let([v1 (eval-under-env (cnd-e1 e) env)])
              (if (bool? v1)
                  (if (equal? (bool-boolean v1) #t)
                      (eval-under-env (cnd-e2 e) env)
                      (eval-under-env (cnd-e3 e) env)
                   )
                   (error "NUMEX not bool")
              ))]

        [(iseq? e)
         (let([v1 (eval-under-env (iseq-e1 e) env)]
              [v2 (eval-under-env (iseq-e2 e) env)])
               (if (and (num? v1)
                        (num? v2))
                   (if (equal? (num-int v1) (num-int v2))(bool #t)(bool #f))
                   (if (and (bool? v1)
                        (bool? v2))
                   (if (equal? (bool-boolean v1) (bool-boolean v2))(bool #t)(bool #f))
                   (if (or(and (bool? v1)
                        (num? v2))(and (bool? v2)
                        (num? v1)))(bool #f) (error "NUMEX not bool and not num isseq"))))
           )]
        ;;;; iseq bool and num ?
        
        [(ifnzero? e)
         (let([v1 (eval-under-env (ifnzero-e1 e) env)])
              (if (num? v1)
                  (if (equal? (num-int v1) 0)
                      (eval-under-env (ifnzero-e3 e) env)
                      (eval-under-env (ifnzero-e2 e) env)
                      )
               (error "NUMEX e1 not number in ifnzero"))   
           )]

        
        [(ifleq? e)
         (let([v1 (eval-under-env (ifleq-e1 e) env)]
              [v2 (eval-under-env (ifleq-e2 e) env)])
              (if (and(num? v1)
                      (num? v2))
                  (if (> (num-int v1) (num-int v2))
                      (eval-under-env (ifleq-e4 e) env)
                      (eval-under-env (ifleq-e3 e) env)
                      )
               (error "NUMEX e1 or e2 not number in ifleq"))   
           )]

        [(with? e) 
          (let ([v1 (eval-under-env (with-e1 e) env)])
           (if (string? (with-s e))
               (eval-under-env (with-e2 e) (cons  (cons (with-s e) v1 ) env))
               (error "NUMEX with applied to non string")))]

         [(lam? e)
           (closure env e)]

         [(apply? e)
          (let ([v1(eval-under-env (apply-funexp e) env)]
                [v2(eval-under-env (apply-actual e) env)])
            (if(closure? v1)
               (let([v3(closure-f v1)])
               ;; (eval-under-env (lam-body v3) (cons(cons((closure-env v1) (lam-nameopt v3)))v2)))
                  (eval-under-env (lam-body v3) (cons(cons(lam-formal v3) v2)(cons(cons (lam-nameopt v3) v1)(closure-env v1))))
               )(error "NUMEX not closure in apply function")))]
         ;;;;; apply

         [(closure? e) e]
         
         [(apair?  e)
          (let ([v1 (eval-under-env (apair-e1 e) env)]
                [v2 (eval-under-env (apair-e2 e) env)])
            (apair v1 v2))]


         [(1st? e)
          (let ([v1 (eval-under-env (1st-e1 e) env)])
                (if (apair? v1)
                    (apair-e1 v1)
                    (error "NUMEX not pair in 1st")
                ))]
         
        [(2nd? e)
          (let ([v1 (eval-under-env (2nd-e1 e) env)])
                (if (apair? v1)
                    (apair-e2 v1)
                    (error "NUMEX not pair in 2nd")
                ))]
        [(munit? e)
           (munit)]
        
        [(ismunit? e)
         (let([v1 (eval-under-env (ismunit-e e) env)])
           (if (equal? v1 (munit))
               (bool #t)
               (bool #f)))]
         
     
        [#t (error (format "bad NUMEX expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifmunit e1 e2 e3)
   (if(munit? e1) e2 e3))

(define (with* bs e2)
  (cond [(null? bs) e2 ]
   [#t (with (car(car bs)) (cdr(car bs)) (with* (cdr bs) e2))]
   ))

(define (ifneq e1 e2 e3 e4)
  (cnd (iseq e1 e2) e4  e3 ))

;; Problem 4

(define numex-filter (lam "function" "mapfun" (lam "map" "numex-list" (cnd (ismunit (var "numex-list"))(munit)
                             (ifnzero (apply (var "mapfun") (1st (var "numex-list")))
                              (apair(apply (var "mapfun") (1st (var "numex-list")))(apply (var "map") (2nd (var "numex-list"))))
                              (apply (var "map") (2nd (var "numex-list")))))))
  )


(define numex-all-gt
  (with "filter" numex-filter
        (lam "function" "i" (apply (var "filter") (lam "greater than" "element" (ifleq (var "element") (var "i") (num 0)(var "element")))))
  ))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e)
  (cond [(var? e) 
         (set(var-string e))]

        [(num? e)
         (set null)]
        
        [(bool? e)
         (set null)]

        
        [(plus? e) 
         (let ([freev1 (compute-free-vars (plus-e1 e))]
               [freev2 (compute-free-vars (plus-e2 e))])
           (set-union freev1 freev2))]
        
        [(div? e) 
         (let ([freev1 (compute-free-vars (div-e1 e))]
               [freev2 (compute-free-vars (div-e2 e))])
           (set-union freev1 freev2))]

        [(minus? e) 
         (let ([freev1 (compute-free-vars (minus-e1 e))]
               [freev2 (compute-free-vars (minus-e2 e))])
           (set-union freev1 freev2))]

        [(mult? e) 
         (let ([freev1 (compute-free-vars (mult-e1 e))]
               [freev2 (compute-free-vars (mult-e2 e))])
           (set-union freev1 freev2))]
        
        [(neg? e)
         (let([freev1 (compute-free-vars (neg-e1 e))])
           freev1)]

         [(andalso? e) 
         (let ([freev1 (compute-free-vars (andalso-e1 e))]
               [freev2 (compute-free-vars (andalso-e2 e))])
           (set-union freev1 freev2))]

         [(orelse? e) 
         (let ([freev1 (compute-free-vars (orelse-e1 e))]
               [freev2 (compute-free-vars (orelse-e2 e))])
           (set-union freev1 freev2))]

         [(cnd? e) 
         (let ([freev1 (compute-free-vars (cnd-e1 e))]
               [freev2 (compute-free-vars (cnd-e2 e))]
               [freev3 (compute-free-vars (cnd-e3 e))])
           (set-union(set-union freev1 freev2) freev3))]
         
         [(iseq? e) 
         (let ([freev1 (compute-free-vars (iseq-e1 e))]
               [freev2 (compute-free-vars (iseq-e2 e))])
               (set-union freev1 freev2))]
         
   
         [(ifnzero? e) 
         (let ([freev1 (compute-free-vars (ifnzero-e1 e))]
               [freev2 (compute-free-vars (ifnzero-e2 e))]
               [freev3 (compute-free-vars (ifnzero-e3 e))])
           (set-union(set-union freev1 freev2) freev3))]
        
        [(ifleq? e) 
         (let ([freev1 (compute-free-vars (ifleq-e1 e))]
               [freev2 (compute-free-vars (ifleq-e2 e))]
               [freev3 (compute-free-vars (ifleq-e3 e))]
               [freev4 (compute-free-vars (ifleq-e4 e))])
           (set-union(set-union(set-union freev1 freev2) freev3)freev4))]

        
        [(with? e)
         (let([freev2 (compute-free-vars (with-e2 e))])
           freev2)]
        
         [(lam? e)
           (let ([freev1 (compute-free-vars lam-body)]
                 [freev2 (compute-free-vars lam-formal)])
             (fun-challenge (lam-nameopt e) (lam-formal e) (lam-body e) (set-remove freev1 freev2)))]
        

         [(apply? e)
         (let([freev1 (compute-free-vars apply-funexp)]
              [freev2 (compute-free-vars apply-actual)])
            (set-union freev1 freev2))]
         
         [(apair? e) 
         (let ([freev1 (compute-free-vars (apair-e1 e))]
               [freev2 (compute-free-vars (apair-e2 e))])
               (set-union freev1 freev2))]
         

         [(1st? e)
         (let([fv1 (compute-free-vars 1st-e1)])
           fv1)]
         
        [(2nd? e)
         (let ([fv1 (compute-free-vars 2nd-e1)])
           fv1)]

        [(munit? e)
           (set(null))]

        [(ismunit? e) 
         (let ([fv1 (compute-free-vars ismunit-e)])
           fv1)]
         
     
        [#t (error (format "bad NUMEX expression: ~v" e))]))

  
  

;; Do NOT share code with eval-under-env because that will make grading
;; more difficult, so copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) (
 cond [(var? e) 
         (envlookup env (var-string e))]

      [(num? e)
         (if (integer? (num-int e)) e (error "NUMEX not number in num struct"))]
        
      [(bool? e)
         (if (boolean? (bool-boolean e)) e (error "NUMEX not boolean in bool struct"))]


      [(plus? e) 
         (let ([v1 (eval-under-env-c (plus-e1 e) env)]
               [v2 (eval-under-env-c (plus-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (+ (num-int v1) 
                       (num-int v2)))
               (error "NUMEX addition applied to non-number")))]
        
        [(minus? e)
         (let([v1 (eval-under-env-c (minus-e1 e) env)]
              [v2 (eval-under-env-c (minus-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (- (num-int v1)
                         (num-int v2)))
               (error "NUMEX minus applied to non-number")))]

        [(mult? e)
         (let([v1 (eval-under-env-c (mult-e1 e) env)]
              [v2 (eval-under-env-c (mult-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (* (num-int v1)
                         (num-int v2)))
               (error "NUMEX multiply applied to non-number")))]

        [(div? e)
         (let([v1 (eval-under-env-c (div-e1 e) env)]
              [v2 (eval-under-env-c (div-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (quotient (num-int v1)
                         (num-int v2)))
               (error "NUMEX divide applied to non-number")))]
;;;;; divide by zero should be handled ?
        [(neg? e)
         (let([v1 (eval-under-env-c (neg-e1 e) env )])
           (if (num? v1)
               (num(-(num-int v1)))
               (if (bool? v1)
               (bool(not(bool-boolean v1)))
               (error "NUMEX not num or boolean"))))]

       
        [(andalso? e)
         (let([v1 (eval-under-env-c (andalso-e1 e) env)])
           (if (bool? v1)
               (if (equal?(bool-boolean v1) #f)
                   (bool #f)
                   (let([v2 (eval-under-env-c (andalso-e2 e) env)])
                     (if (bool? v2)
                     (if (equal? (bool-boolean v2) #f)(bool #f)
                         (bool #t))(error "NUMEX not boolean"))))(error "NUMEX not bool")))]

        [(orelse? e)
         (let([v1 (eval-under-env-c (orelse-e1 e) env)])
           (if (bool? v1)
               (if (equal?(bool-boolean v1) #t)
                   (bool #t)
                   (let([v2 (eval-under-env-c (orelse-e2 e) env)])
                     (if (bool? v2)
                     (if (equal? (bool-boolean v2) #f)(bool #f)
                         (bool #t))(error "NUMEX not boolean"))))(error "NUMEX not bool")))]

        [(cnd? e)
         (let([v1 (eval-under-env-c (cnd-e1 e) env)])
              (if (bool? v1)
                  (if (equal? (bool-boolean v1) #t)
                      (eval-under-env (cnd-e2 e) env)
                      (eval-under-env (cnd-e3 e) env)
                   )
                   (error "NUMEX not bool")
              ))]

        [(iseq? e)
         (let([v1 (eval-under-env-c (iseq-e1 e) env)]
              [v2 (eval-under-env-c (iseq-e2 e) env)])
               (if (and (num? v1)
                        (num? v2))
                   (if (equal? (num-int v1) (num-int v2))(bool #t)(bool #f))
                   (if (and (bool? v1)
                        (bool? v2))
                   (if (equal? (bool-boolean v1) (bool-boolean v2))(bool #t)(bool #f))
                   (if (or(and (bool? v1)
                        (num? v2))(and (bool? v2)
                        (num? v1)))(bool #f) (error "NUMEX not bool and not num isseq"))))
           )]
        ;;;; iseq bool and num ?
        
        [(ifnzero? e)
         (let([v1 (eval-under-env-c (ifnzero-e1 e) env)])
              (if (num? v1)
                  (if (equal? (num-int v1) 0)
                      (eval-under-env (ifnzero-e3 e) env)
                      (eval-under-env (ifnzero-e2 e) env)
                      )
               (error "NUMEX e1 not number in ifnzero"))   
           )]

        
        [(ifleq? e)
         (let([v1 (eval-under-env-c (ifleq-e1 e) env)]
              [v2 (eval-under-env-c (ifleq-e2 e) env)])
              (if (and(num? v1)
                      (num? v2))
                  (if (> (num-int v1) (num-int v2))
                      (eval-under-env (ifleq-e4 e) env)
                      (eval-under-env (ifleq-e3 e) env)
                      )
               (error "NUMEX e1 or e2 not number in ifleq"))   
           )]

        [(with? e) 
          (let ([v1 (eval-under-env-c (with-e1 e) env)])
           (if (string? (with-s e))
               (eval-under-env (with-e2 e) (cons  (cons (with-s e) v1 ) env))
               (error "NUMEX with applied to non string")))]

         [(fun-challenge? e)
           (closure env e)]

         [(apply? e)
          (let ([v1(eval-under-env-c (apply-funexp e) env)]
                [v2(eval-under-env-c (apply-actual e) env)])
            (if(closure? v1)
               (let([v3(closure-f v1)])
                 ;; (eval-under-env (lam-body v3) (cons(cons((closure-env v1) (lam-nameopt v3)))v2)))
                  (eval-under-env (lam-body v3) (cons(cons(lam-formal v3) v2)(cons(cons (lam-nameopt v3) v1)(closure-env v1))))
               )(error "NUMEX not closure in apply function")))]
         
         [(closure? e) e]
         
         [(apair?  e)
          (let ([v1 (eval-under-env-c (apair-e1 e) env)]
                [v2 (eval-under-env-c (apair-e2 e) env)])
            (apair v1 v2))]


         [(1st? e)
          (let ([v1 (eval-under-env-c (1st-e1 e) env)])
                (if (apair? v1)
                    (apair-e1 v1)
                    (error "NUMEX not pair in 1st")
                ))]
         
        [(2nd? e)
          (let ([v1 (eval-under-env-c (2nd-e1 e) env)])
                (if (apair? v1)
                    (apair-e2 v1)
                    (error "NUMEX not pair in 2nd")
                ))]
        [(munit? e)
           (munit)]
        
        [(ismunit? e)
         (let([v1 (eval-under-env-c (ismunit-e e) env)])
           (if (equal? v1 (munit))
               (bool #t)
               (bool #f)))]
         
     
        [#t (error (format "bad NUMEX expression: ~v" e))]))

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
