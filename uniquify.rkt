#lang racket

(require "utilities.rkt")
(require "interp-Lwhile.rkt")
(require "type-check-Lwhile.rkt")

(provide uniquify)

(define (uniquify-exp env e)
    (match e
      [(Var x)
       (Var (dict-ref env x))]
      [(Int n) (Int n)]
      [(Bool b) (Bool b)]
      [(Void) (Void)]
      [(SetBang x e)
       (SetBang (dict-ref env x) (uniquify-exp env e))]
      [(Begin es body)
       (Begin (map (λ (e) (uniquify-exp env e)) es) (uniquify-exp env body))]
      [(WhileLoop cnd body)
       (WhileLoop (uniquify-exp env cnd) (uniquify-exp env body))]
      [(If e1 e2 e3) (If (uniquify-exp env e1) (uniquify-exp env e2) (uniquify-exp env e3))]
      [(Let x e body)
       (define x2 (gensym x))
       (define env2 (dict-set env x x2)) 
       (Let x2 (uniquify-exp env e) (uniquify-exp env2 body))]
      [(Prim op es)
       (Prim op (for/list ([e es]) (uniquify-exp env e)))]))

;; uniquify : Lvar -> Lvar
(define (uniquify p)
  (match p
    [(Program info e) (Program info (uniquify-exp '() e))]))
