#lang racket

(require "interp-Lif.rkt")
(require "type-check-Lif.rkt")
(require "utilities.rkt")

(define basic-blocks '())

(define (create-block tail)
  (match tail
    [(Goto label) (Goto label)]
    [else
     (let ([label (gensym 'block)])
       (set! basic-blocks (cons (cons label tail) basic-blocks)) (Goto label))]))


(define (explicate-tail e)
  (match e
    [(Var x) (Return (Var x))]
    [(Int n) (Return (Int n))]
    [(Let x rhs body) (explicate_assign rhs x (explicate_tail body))]
    [(Prim op es)
     (define x (gensym "x"))
     (Seq (Assign (Var x) (Prim op es)) (Return (Var x)))]
    [else (error "explicate_tail unhandled case" e)]))
  
(define (explicate-assign e x cont)
  (match e
    [(Var y) (Seq (Assign (Var x) (Var y)) cont)]
    [(Int n) (Seq (Assign (Var x) (Int n)) cont)]
    [(Let y rhs body)
     (explicate_assign rhs y (explicate_assign body x cont))]
    [(Prim op es) (Seq (Assign (Var x) (Prim op es)) cont)]
    [else (error "eplicate_assign unhandled case" e)]))


(define (explicate-control p)
  (match p
    [(Program info body)
     (define labels `((start . ,(explicate-tail body))))
     (CProgram info labels)]))

