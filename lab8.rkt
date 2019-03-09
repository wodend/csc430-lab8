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
;; determine addresses referred to by a value
(: val-addrs (-> Value (Listof Address)))
(define (val-addrs val)
  (match val
    [(ArrayV a #{l : Integer})
     #{(build-list l (lambda ([x : Index]) (+ a x)))
       : (Listof Nonnegative-Integer)}]
    [(ClosureV ps b e) (map (lambda (x) (hash-ref e x)) ps)]
    [_ '()]))

;; determine addresses referred to by an environment
(: env-addrs (-> Envir (Listof Address)))
(define (env-addrs env)
  (remove-duplicates (hash-values env)))

;; mark live addresses
(: mark (-> (Listof Address) Store (Listof Address)))
(define (mark uns store)
  (match uns
    ['() '()]
    [`(,x . ,xs) (cons x (mark (append (val-addrs (hash-ref store x)) xs)
                               store))]))

;; sweep for garbage
(: sweep (-> Store (Listof Address) (Listof Address)))
(define (sweep store live)
  (let ([as (list->set (hash-keys store))]
        [l (list->set live)])
    (set->list (set-subtract as l))))

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
                                                (1 . 1))))
(define store03 : Store  (make-immutable-hash `((0 . 1)
                                                (1 . ,(ArrayV 2 1))
                                                (2 . 1))))
(check-equal? (val-addrs 0) '())
(check-equal? (val-addrs (ArrayV 0 2)) '(0 1))
(check-equal? (val-addrs (ClosureV '() '0 env00)) '())
(check-equal? (val-addrs (ClosureV '(x y) 'y env02)) '(0 1))
(check-equal? (env-addrs env00) '())
(check-equal? (env-addrs env02) '(0 1))
(check-equal? (sweep store00 '()) '())
(check-equal? (sweep store02 '(0)) '(1))
(check-equal? (mark (env-addrs env00) store00) '())
(check-equal? (mark (env-addrs env02) store03) '(0 1 2))
(check-exn #rx"ZHRL: unimplemented" (lambda () (collect `(,env00) store00)))