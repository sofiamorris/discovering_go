#lang typed/racket

(require typed/rackunit)

;; macro to convert tstruct to transparent structs
(define-syntax tstruct
  (syntax-rules ()
    [(_ name fields)
     (struct name fields #:transparent)]))

(provide tstruct)

;; Store type (same as AAQZ)
(define-type Sto (Mutable-Vectorof Value))

;; Go expression types
(define-type ExprC (U NumC 
                     IdC 
                     StrC 
                     AppC 
                     LamC
                     LetC      ; For variable declarations
                     ForC      ; For loops
                     StructC   ; Struct definitions
                     MethodC   ; Method definitions
                     FieldC    ; Field access
                     PtrC      ; Pointer operations
                     SliceC))  ; Slice operations

;; Basic expression constructors
(tstruct NumC ([n : Real]))
(tstruct IdC ([sym : Symbol]))
(tstruct StrC ([str : String]))
(tstruct AppC ([fun : ExprC] [args : (Listof ExprC)]))
(tstruct LamC ([args : (Listof Symbol)] [body : ExprC] [param-types : (Listof Type)]))

;; Go-specific expressions
(tstruct LetC ([name : Symbol] [init : ExprC] [body : ExprC]))
(tstruct ForC ([init : ExprC] [cond : ExprC] [inc : ExprC] [body : ExprC]))
(tstruct StructC ([name : Symbol] [fields : (Listof (Pair Symbol Type))]))
(tstruct MethodC ([recv : Symbol] [name : Symbol] [args : (Listof Symbol)] [body : ExprC]))
(tstruct FieldC ([obj : ExprC] [field : Symbol]))
(tstruct PtrC ([op : Symbol] [expr : ExprC])) ; For & and * operations
(tstruct SliceC ([arr : ExprC] [start : ExprC] [end : ExprC]))

;; Go value types 
(define-type Value (U NumV 
                     BoolV 
                     StrV 
                     PrimV 
                     ClosV
                     StructV   ; Struct instances
                     SliceV    ; Slice values  
                     PtrV      ; Pointer values
                     NilV      ; nil value
                     ArrayV))  ; Array values

(tstruct NumV ([val : Real]))
(tstruct BoolV ([val : Boolean]))
(tstruct StrV ([val : String]))
(tstruct PrimV ([sym : Symbol]))
(tstruct ClosV ([args : (Listof Symbol)] [body : ExprC] [env : Env]))
(tstruct StructV ([type-name : Symbol] [fields : (HashTable Symbol Value)]))
(tstruct SliceV ([elements : (Vectorof Value)] [start : Integer] [len : Integer] [cap : Integer]))
(tstruct PtrV ([addr : Natural]))
(tstruct NilV ())
(tstruct ArrayV ([elements : (Vectorof Value)]))

;; Type system
(define-type Type (U NumT BoolT StrT FunT StructT SliceT PtrT))
(tstruct NumT ())
(tstruct BoolT ())
(tstruct StrT ())
(tstruct FunT ([params : (Listof Type)] [body : Type]))
(tstruct StructT ([name : Symbol] [fields : (HashTable Symbol Type)]))
(tstruct SliceT ([element-type : Type]))
(tstruct PtrT ([target-type : Type]))

;; Helper function to generate initial store 
(: make-initial-store (Natural -> Sto))
(define (make-initial-store memsize)
  (define make-value-vector (inst make-vector Value))
  (define store (make-value-vector memsize (NilV)))
  (vector-set! store (ann 0 Natural) (NumV 9)) ; Store size at index 0
  (vector-set! store (ann 1 Natural) (PrimV '+))
  (vector-set! store (ann 2 Natural) (PrimV '-))
  (vector-set! store (ann 3 Natural) (PrimV '*))
  (vector-set! store (ann 4 Natural) (PrimV '/))
  store)

;; Environment types remain similar
(tstruct Binding ([name : Symbol] [addr : Natural]))
(define-type Env (Listof Binding))

;; consumes an ExprC and an environment and interps its value
#;(: interp (ExprC Env Sto -> Value))
#;(define (interp expr curr-env sto)
  (match expr
    [(NumC n) (NumV n)]
    [(StrC str) (StrV str)]
    [(IdC n) (lookup-env n curr-env sto)]
    [(RecC name _ rhs body) (local ([define addr (allocate 1 (list (InvalidV)) sto)]
                                    [define new-env (cons (Binding name addr) curr-env)]
                                    [define rhs-val (interp rhs new-env sto)])
                              (vector-set! sto addr rhs-val)
                              (interp body new-env sto))]
    [(LamC params body p-types) (ClosV params body curr-env)]
    [(AppC fun args) 
     (local ([define fun-val (interp fun curr-env sto)])
       (match fun-val
         [(PrimV s) (interp-primv s args curr-env sto)]
         [(ClosV params body clos-env) (local [(define arg-vals (map (λ ([exp : ExprC])
                                                                       (interp exp curr-env sto))
                                                                     args))]
                                         (if (equal? (length params) (length arg-vals))
                                             (interp body
                                                     (foldl (λ ([param : Symbol] [arg : Value] [env : Env])
                                                              (cons (Binding param (allocate 1 (list arg) sto)) env))
                                                            clos-env
                                                            params
                                                            arg-vals)
                                                     sto)
                                             (error 'interp
                                                    "AAQZ arity mismatch between params and args for function")))]
         [other (error 'interp "GOLAN cannot apply value function ~e for fun ~e ~nstore: ~v" fun-val fun sto)]))]
    [(IfC cond if-true if-false) (match (interp cond curr-env sto)
                                   [(BoolV tf) (if tf
                                                   (interp if-true curr-env sto)
                                                   (interp if-false curr-env sto))]
                                   [other (error 'interp "GOLAN expected boolean condition, got: ~v" other)])]
    #;[other (error 'interp "unimplemented ~v" other)]))

;; Main interpreter for primitive operations
#;(: interp-primv (Symbol (Listof ExprC) Env Sto -> Value))
#;(define (interp-primv sym args curr-env sto)
  (define arg-vals (map (λ ([exp : ExprC])
                          (interp exp curr-env sto))
                        args))
  (match sym
    ['+ (apply-binop + sym arg-vals)]
    ['- (apply-binop - sym arg-vals)]
    ['* (apply-binop * sym arg-vals)]
    ['/ (match (check-two-nums sym arg-vals)
          [(cons left right)
           (if (zero? (NumV-val right))
               (error 'interp-primv "GOLAN division by zero: ~e" arg-vals)
               (NumV (/ (NumV-val left) (NumV-val right))))])]
    ['<= (apply-compare <= sym arg-vals)]
    ['num-eq? (match (check-two-nums sym arg-vals)
                [(cons left right)
                 (BoolV (= (NumV-val left)(NumV-val right)))])]
    #;['str-eq? (match arg-vals
                  [(list left right)
                   (if (and (StrV? left) (StrV? right))
                       (BoolV (string=? (StrV-val left) (StrV-val right)))
                       (error 'interp-primv 
                              "GOLAN attempted to compare non-string values with str-eq?: ~e"
                              arg-vals))]
                  [other (error 'interp-primv 
                                "GOLAN two arguments required for str-eq?, provided: ~e"
                                arg-vals)])]
    ['str-eq? (match arg-vals
                [(list (? StrV? left) (? StrV? right)) (BoolV (string=? (StrV-val left) (StrV-val right)))])]
    #;['equal? (match arg-vals
                 [(list (ArrayV addr1 _) (ArrayV addr2 _)) (BoolV (equal? addr1 addr2))]
                 [(list left right) (if (and (not (or (ClosV? left) (PrimV? left)))
                                             (not (or (ClosV? right) (PrimV? right))))
                                        (BoolV (equal? left right))
                                        (BoolV #f))]
                 [other (error 'interp-primv
                               "GOLAN two arguments required for equal? comparison, provided: ~e" arg-vals)])]
    [other (error 'interp-primv "GOLAN unimplemented PrimV type: ~e" other)]))

;; Store operations
(: lookup-sto (Natural Sto -> Value))
(define (lookup-sto addr sto)
  (match (vector-ref sto (ann 0 Natural))
    [(NumV val) (if (< addr val)
                    (vector-ref sto addr)
                    (error 'lookup-sto "GOLAN address ~v out of bounds" addr))]
    [other (error 'lookup-sto "GOLAN corrupted store")]))

;; Allocate operations
(: allocate (Natural (Listof Value) Sto -> Natural))
(define (allocate num-vals vals sto)
  (match (vector-ref sto (ann 0 Natural))
    [(NumV (? natural? num-sto-vals))
     (if (>= (vector-length sto) (+ num-sto-vals num-vals))
         (begin 
           (vector-set! sto (ann 0 Natural) (NumV (+ num-vals num-sto-vals)))
           (set-store-vals num-sto-vals vals sto)
           num-sto-vals)
         (error 'allocate "GOLAN out of memory"))]
    [other (error 'allocate "GOLAN corrupted store")]))

;; Helper to set multiple values in store
(: set-store-vals (Natural (Listof Value) Sto -> Void))
(define (set-store-vals addr vals sto)
  (match vals
    ['() (void)]
    [(cons first rest) 
     (vector-set! sto addr first)
     (set-store-vals (+ 1 addr) rest sto)]))