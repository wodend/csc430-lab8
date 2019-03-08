#lang typed/racket

(require typed/rackunit)

;; # Types, Data Definitions, and Global Helper Functions
;; raises a ZHRL error
(: zhrl-err (-> String Symbol * Nothing))
(define (zhrl-err mess . syms)
  (error (string-append "ZHRL: " mess) syms))

;; represents an expression
(define-type ExpressC (U Real Symbol String LamC ApplyC IfC SetC))
(struct LamC ([pars : (Listof Symbol)]
              [body : ExpressC]) #:transparent)
(struct ApplyC ([func : ExpressC]
                [args : (Listof ExpressC)]) #:transparent)
(struct IfC ([tesc : ExpressC]
             [thec : ExpressC]
             [elsc : ExpressC]) #:transparent)
(struct SetC ([iden : Symbol]
              [valu : ExpressC]))

;; represents a value
(define-type Value (U Real Boolean String ClosureV PrimitiV ArrayV))
(struct ClosureV ([pars : (Listof Symbol)]
                  [body : ExpressC]
                  [envi : Environ]))
(define-type PrimitiV (U '+ '- '* '/))
(struct ArrayV ([addr : Address] [leng : Natural])
        #:transparent)

;; represents an environment
(define-type Environ (Immutable-HashTable Symbol Address))

;; represents an address
(define-type Address Natural)

;; represents a store
(define-type Store (Immutable-HashTable Address Value))

;; collect unreachable memory in the store
(: collect (-> (Listof Environ) Store (Listof Address)))
(define (collect envs stor)
  (zhrl-err "unimplemented"))

;; # Tests
(define envirt00 : Environ (make-immutable-hash '()))
(define envirt01 : Environ (make-immutable-hash '((x . 0))))
(define envirt02 : Environ (make-immutable-hash '((x . 0)
                                                  (y . 1))))
(define storet00 : Store  (make-immutable-hash '()))
(define storet01 : Store  (make-immutable-hash '((0 . 1))))
(define storet02 : Store  (make-immutable-hash '((0 . 1)
                                                 (1 . 0))))
(define storet03 : Store  (make-immutable-hash `((0 . ,(ArrayV 1 1))
                                                 (1 . 1)
                                                 (2 . 0))))

(check-exn #rx"ZHRL: unimplemented" (lambda () (collect `(,envirt00) storet00)))