#lang racket

(require "interp-Lif.rkt")
(require "type-check-Lif.rkt")
(require "utilities.rkt")

(provide explicate-control)

(define basic-blocks '())

(define (create-block tail [label (gensym 'label)])
  (match tail
    [(Goto l) (Goto l)]
    [else
       (set! basic-blocks (cons (cons label tail) basic-blocks))
       (Goto label)]))

(define (explicate-tail e)
  (match e
    [(Var x) (Return (Var x))]
    [(Int n) (Return (Int n))]
    [(Bool b) (Return (Bool b))]
    [(Void) (Return (Void))]
    [(Collect n) (Seq (Collect n) (Return (Void)))]
    [(Allocate n ty) (Return (Allocate n ty))]
    [(GlobalValue var) (Return (GlobalValue var))]
    [(GetBang x) (Return (Var x))]
    [(SetBang x e)
     (Seq (Assign (Var x) e) (Return (Void)))]
    [(Begin es body)
     (for/fold
      ([t (explicate-tail body)])
      ([e (reverse es)])
       (explicate-effect e t))]
    [(WhileLoop cnd body)
     (define loop-label (gensym 'loop))
     (define body^ (create-block (explicate-effect body (Goto loop-label))))
     (create-block (explicate-pred cnd body^ (Return (Void))) loop-label)]
    [(If e1 e2 e3) (explicate-pred e1 (explicate-tail e2) (explicate-tail e3))]
    [(Let x rhs body) (explicate-assign rhs x (explicate-tail body))]
    [(Prim op es)
     (define x (gensym "x"))
     (Seq (Assign (Var x) (Prim op es)) (Return (Var x)))]
    [else (error "explicate_tail unhandled case" e)]))
  
(define (explicate-assign e x cont)
  (match e
    [(Var y) (Seq (Assign (Var x) (Var y)) cont)]
    [(Int n) (Seq (Assign (Var x) (Int n)) cont)]
    [(Bool b) (Seq (Assign (Var x) (Bool b)) cont)]
    [(Void) (Seq (Assign (Var x) (Void)) cont)]
    [(Collect n) (Seq (Collect n) (Seq (Assign (Var x) (Void)) cont))]
    [(Allocate n ty) (Seq (Assign (Var x) (Allocate n ty)) cont)]
    [(GlobalValue var) (Seq (Assign (Var x) (GlobalValue var)) cont)]
    [(GetBang y) (Seq (Assign (Var x) (Var y)) cont)]
    [(SetBang y e)
     (define cont^ (Seq (Assign (Var x) (Void)) cont))
     (explicate-assign e y cont^)]
    [(Begin es body)
     (define cont^ (explicate-assign body x cont))
     (for/fold
      ([cont^ (explicate-assign body x cont)])
      ([e es])
       (explicate-effect e cont^))]
    [(WhileLoop cnd body)
     (define loop-label (gensym 'loop))
     (define body^ (create-block (explicate-effect body (Goto loop-label))))
     (create-block (explicate-pred cnd body^ (Seq (Assign (Var x) (Void)) cont)) loop-label)]
    [(If e1 e2 e3)
     (define cont^ (create-block cont))
     (explicate-pred
      e1
      (explicate-assign e2 x cont^)
      (explicate-assign e3 x cont^))]
    [(Let y rhs body)
     (explicate-assign rhs y (explicate-assign body x cont))]
    [(Prim op es) (Seq (Assign (Var x) (Prim op es)) cont)]
    [else (error "explicate_assign unhandled case" e)]))

(define (explicate-pred cond true-tail false-tail)
  (match cond
    [(Var x)
     (IfStmt
      (Prim 'eq? (list (Var x) (Bool #t)))
      (create-block true-tail)
      (create-block false-tail))]
    [(Prim 'vector-ref (list vec ind))
     (define x (gensym "x"))
     (Seq
      (Assign (Var x) (Prim 'vector-ref (list vec ind)))  
      (IfStmt
       (Prim 'eq? (list (Var x) (Bool #t)))
       (create-block true-tail)
       (create-block false-tail)))]
    [(GetBang x)
     (IfStmt
      (Prim 'eq? (list (Var x) (Bool #t)))
      (create-block true-tail)
      (create-block false-tail))]
    [(Begin es body)
     (for/fold
      ([cond^ (explicate-pred body true-tail false-tail)])
      ([e es])
      (explicate-effect e cond^))]
    [(Let x rhs body)
     (explicate-assign rhs x (explicate-pred body true-tail false-tail))]
    [(Prim 'not (list e)) (explicate-pred e false-tail true-tail)]
    [(Prim op es) #:when (member op (list 'eq? '< '<= '> '>=))
                  (IfStmt (Prim op es) (create-block true-tail)
                          (create-block false-tail))]
    [(Bool b) (if b true-tail false-tail)]
    [(If cond^ true-tail^ false-tail^)
     (define t-block (create-block true-tail))
     (define f-block (create-block false-tail))
     (define t (explicate-pred true-tail^ t-block f-block))
     (define f (explicate-pred false-tail^ t-block f-block))
     (explicate-pred cond^ t f)]
    [else (error "explicate-pred unhandled case" cond)]))

(define (explicate-effect e cont)
  (match e
    [(Var x) cont]
    [(Int n) cont]
    [(Bool b) cont]
    [(Collect n)
     (Seq (Collect n) cont)]
    [(Prim 'vector-set! (list vec ind val))
     (Seq (Prim 'vector-set! (list vec ind val)) cont)]
    [(Prim op es) cont] ; TODO: we need to handle the read primitive here
    [(Void) cont]
    [(Let x rhs body)
     (explicate-assign rhs x (explicate-effect body cont))]
    [(GetBang x) cont]
    [(SetBang x e)
     (explicate-assign e x cont)]
    [(Begin es body)
     (for/fold
      ([cont^ (explicate-effect body cont)])
      ([e es])
      (explicate-effect e cont^))]
    [(WhileLoop cnd body)
     (define loop-label (gensym 'loop))
     (define body^ (create-block (explicate-effect body (Goto loop-label))))
     (create-block (explicate-pred cnd body^ cont) loop-label)]
    [else (error "explicate-effect unhandled case" e)]))

(define (explicate-control p)
  (set! basic-blocks '())
  (match p
    [(Program info body)
     (define labels (cons `(start . ,(explicate-tail body)) basic-blocks))
     (CProgram info labels)]))

