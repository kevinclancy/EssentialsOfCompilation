#lang racket

(require "interp-Lwhile.rkt")
(require "type-check-Lwhile.rkt")
(require "utilities.rkt")

(provide shrink)

(define (shrink-exp e)
  (match e
    [(Prim 'and [list e1 e2])
     (If e1 e2 (Bool #f))]
    [(Prim 'or [list e1 e2])
     (If e1 (Bool #t) e2)]
    [(Let x e body)
     (Let x (shrink-exp e) (shrink-exp body))]
    [(If e1 e2 e3)
     (If (shrink-exp e1) (shrink-exp e2) (shrink-exp e3))]
    [(Prim op es)
     (Prim op (for/list ([e es]) (shrink-exp e)))]
    [(SetBang x e)
     (SetBang x (shrink-exp e))]
    [(Begin es body)
     (Begin (map shrink-exp es) (shrink-exp body))]
    [(WhileLoop cnd body)
     (WhileLoop (shrink-exp cnd) (shrink-exp body))]
    [else e]))
  
(define (shrink p)
  (match p
    [(Program info e)
     (Program info (shrink-exp e))]))
