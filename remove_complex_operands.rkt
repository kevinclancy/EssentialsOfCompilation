#lang racket

(require "utilities.rkt")

(provide remove-complex-operands)

;; the argument is already atomic, so we leave it unchanged
(struct atomic-arg (e))
  
;; bind the (non-atomic) argument to a variable so that we can use the (atomic) variable in its place
(struct bind-arg (x e))

(define (get-arg a)
  (match a
    [(atomic-arg e) e]
    [(bind-arg x e) (Var x)]))
  
; given bindings=[(x1 . e1) (x2 . e2) ... (xn . en)],
; return an expression (Let x1 e1 (Let x2 e2 ...body)...)
(define (multi-let bindings body)
  (match bindings
    [(cons (cons x e) rest) (Let x e (multi-let rest body))]
    [(list) body]))
  
(define (rco_atom e)
  (match e
    [(Var x) (atomic-arg (Var x))]
    [(Int n) (atomic-arg (Int n))]
    [(Bool b) (atomic-arg (Bool b))]
    [(Void) (atomic-arg (Void))]
    [(GetBang var)
     (define x2 (gensym "x"))
     (bind-arg x2 (GetBang var))]
    [(SetBang var exp)
     (define x2 (gensym "x"))
     (bind-arg x2 (SetBang var (rco_exp exp)))]
    [(Begin es body)
     (define x2 (gensym "x"))
     (bind-arg x2 (Begin (map rco_exp es) (rco_exp body)))]
    [(WhileLoop cnd body)
     (define x2 (gensym "x"))
     (bind-arg x2 (WhileLoop (rco_exp cnd) (rco_exp body)))]
    [(Let x e body)
     (define x2 (gensym "x"))
     (bind-arg x2 (Let x (rco_exp e) (rco_exp body)))]
    [(If e1 e2 e3)
     (define x2 (gensym "x"))
     (bind-arg x2 (If (rco_exp e1) (rco_exp e2) (rco_exp e3)))]
    [(Prim op es)
     (define x2 (gensym "x"))
     (define arg_results (for/list ([e es]) (rco_atom e)))
     (define args (map get-arg arg_results))
     (define bindings
       (for/list ([res arg_results] #:when (bind-arg? res))
         (cons (bind-arg-x res) (bind-arg-e res))))
     (bind-arg x2 (multi-let bindings (Prim op args)))]))

(define (rco_exp e)
  (match e
    [(Var x) (Var x)]
    [(Int n) (Int n)]
    [(Bool b) (Bool b)]
    [(Void) (Void)]
    [(Let x e body) (Let x (rco_exp e) (rco_exp body))]
    [(If e1 e2 e3) (If (rco_exp e1) (rco_exp e2) (rco_exp e3))]
    [(GetBang var) (GetBang var)]
    [(SetBang var exp) (SetBang var (rco_exp exp))]
    [(Begin es body) (Begin (map rco_exp es) (rco_exp body))]
    [(WhileLoop cnd body) (WhileLoop (rco_exp cnd) (rco_exp body))]
    [(Prim op es)
     ;todo: is there a standard library "unzip" function? use that here.
     (define arg_results (for/list ([e es]) (rco_atom e)))
     (define args
       (for/list ([res arg_results])
         (match res
           [(atomic-arg e) e]
           [(bind-arg x e) (Var x)])))
     (define bindings
       (for/list ([res arg_results] #:when (bind-arg? res))
         (cons (bind-arg-x res) (bind-arg-e res))))
     (multi-let bindings (Prim op args))]))

;; remove-complex-opera* : Lvar -> Lvar^mon
(define (remove-complex-operands p)
  (match p
    [(Program info e) (Program info (rco_exp e))]))
