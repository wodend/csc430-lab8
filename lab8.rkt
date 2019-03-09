#lang typed/racket

(require typed/rackunit)

;; # Types, Data Definitions, and Global Helper Functions
;; raises a ZHRL error
(: err-zhrl (-> String Symbol * Nothing))
(define (err-zhrl mess . syms)
  (error (string-append "ZHRL: " mess) syms))

;; represents an expression
(define-type ExpressC (U Real Symbol String LamC ApplyC IfC SetC))
(struct LamC ([pars : (Listof Symbol)]
              [body : ExpressC]) #:transparent)
(struct ApplyC ([func : ExpressC]
                [args : (Listof ExpressC)]) #:transparent)
(struct IfC ([testc : ExpressC]
             [thenc : ExpressC]
             [elsec : ExpressC]) #:transparent)
(struct SetC ([id : Symbol]
              [val : ExpressC]))

;; represents a value
(define-type Value (U Real Boolean String ClosureV PrimitV ArrayV))
(struct ClosureV ([pars : (Listof Symbol)]
                  [body : ExpressC]
                  [env : Envir]))
(define-type PrimitV (U '+ '- '* '/))
(struct ArrayV ([addr : Address] [len : Natural])
        #:transparent)

;; represents an environment
(define-type Envir (Immutable-HashTable Symbol Address))

;; represents an address
(define-type Address Natural)

;; represents a store
(define-type Store (Immutable-HashTable Address Value))

;; # Garbage Collector
;; determine addresses seen by values 
(: seen-val (-> Value (Listof Address)))
(define (seen-val val)
  (match val
    [(ArrayV a #{l : Integer})
     #{(build-list l (lambda ([x : Index]) x)) : (Listof Nonnegative-Integer)}]
    [_ '()]))

;; determine addresses seen by environments
(: seen-env (-> Envir (Listof Address)))
(define (seen-env env)
  (remove-duplicates (hash-values env)))

;; collect unreachable memory in the store
(: collect (-> (Listof Envir) Store (Listof Address)))
(define (collect envs store)
  (err-zhrl "unimplemented"))

;; # Tests
(define env00 : Envir (make-immutable-hash '()))
(define env01 : Envir (make-immutable-hash '((x . 0))))
(define env02 : Envir (make-immutable-hash '((x . 0)
                                             (y . 1))))
(define store00 : Store  (make-immutable-hash '()))
(define store01 : Store  (make-immutable-hash '((0 . 1))))
(define store02 : Store  (make-immutable-hash '((0 . 1)
                                                (1 . 0))))
(define store03 : Store  (make-immutable-hash `((0 . ,(ArrayV 1 1))
                                                (1 . 1)
                                                (2 . 0))))

(check-equal? (seen-val 0) '())
(check-equal? (seen-val (ArrayV 0 2)) '(0 1))
(check-equal? (seen-env env00) '())
(check-equal? (seen-env env02) '(0 1))
(check-exn #rx"ZHRL: unimplemented" (lambda () (collect `(,env00) store00)))