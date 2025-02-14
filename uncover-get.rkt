#lang racket

(require "type-check-Lif.rkt")
(require "interp-Lif.rkt")
(require "utilities.rkt")

(provide uncover-get)

(define (collect-set! e)
  (match e
    [(Var x) (set)]
    [(Int n) (set)]
    [(Bool b) (set)]
    [(Prim op es)
     (for/fold ([acc (set)]) ([e es])
       (set-union (collect-set! e) acc))]
    [(Let x rhs body)
     (set-union (collect-set! rhs) (collect-set! body))]
    [(If cnd then-clause else-clause)
     (set-union
      (collect-set! cnd)
      (collect-set! then-clause)
      (collect-set! else-clause))]
    [(SetBang var rhs)
     (set-union (set var) (collect-set! rhs))]
    [(Begin es body)
     (for/fold ([acc (collect-set! body)]) ([e es])
       (set-union (collect-set! e) acc))]
    [(WhileLoop cnd body)
     (set-union (collect-set! cnd) (collect-set! body))]
    [(Void) (set)]))

(define (uncover-get!-exp mutable-vars e)
  (define (uncover-get-aux e)
    (match e
      [(Var x)
       (if (set-member? mutable-vars x)
           (GetBang x)
           (Var x))]
      [(Int n) (Int n)]
      [(Bool b) (Bool b)]
      [(Prim op es)
       (Prim op (map uncover-get-aux es))]
      [(Let x rhs body)
       (Let
        x
        (uncover-get-aux rhs)
        (uncover-get-aux body))]
      [(If cnd then-clause else-clause)
       (If
        (uncover-get-aux cnd)
        (uncover-get-aux then-clause)
        (uncover-get-aux else-clause))]
      [(SetBang var rhs)
       (SetBang var (uncover-get-aux rhs))]
      [(Begin es body)
       (Begin (map uncover-get-aux es) (uncover-get-aux body))]
      [(WhileLoop cnd body)
       (WhileLoop (uncover-get-aux cnd) (uncover-get-aux body))]
      [(Void) (Void)]))
  (uncover-get-aux e))

(define (uncover-get p)
  (match p
    [(Program '() exp)
     (define mutable-vars (collect-set! exp))
     (Program '() (uncover-get!-exp mutable-vars exp))]))