#lang racket

(require "utilities.rkt")

(provide expose-allocation)

(define (letchain es xs body)
  (for/fold ([b body])
            ([e es] [x xs])
    (Let x e b)))

(define (setchain v n xs)
  (for/fold ([b (Var v)])
            ([x xs] [i (range n)]) 
    (Let (gensym 'u)
         (Prim 'vector-set! (list (Var v) (Int i) (Var x)))
         b)))

;; 
;;   (define (setchain-aux n xs)
;;     (if (= -1 n)
;;         (Var v)
;;         (Let (gensym 'u)
;;              (Prim 'vector-set! (list (Var v) (Int n) (Var (first xs))))
;;              (setchain-aux (- n 1) (rest xs)))))
;;   (setchain-aux (- n 1) xs))

(define (expose-alloc-exp e)
  (match e
     [(HasType (Prim 'vector es) ty)
      (define v (gensym 'v))
      (define n (length es))
      (define es^ (map expose-alloc-exp es))
      (define bytes (+ 8 (* n 8)))
      (define xs (reverse (for/list ([e es^]) (gensym 'x))))
      (letchain
       es^
       xs
       (Let (gensym 'u)
            (If (Prim '< (list (Prim '+
                                     (list (GlobalValue 'free_ptr) (Int bytes)))
                               (GlobalValue 'fromspace_end)))
                (Void)
                (Collect bytes))
            (Let v (Allocate n ty) (setchain v n xs))))]
    [(Var x) (Var x)]
    [(Int n) (Int n)]
    [(Bool b) (Bool b)]
    [(Void) (Void)]
    [(Let x e body)
     (Let x (expose-alloc-exp e) body)]
    [(If e1 e2 e3)
     (If (expose-alloc-exp e1) (expose-alloc-exp e2) (expose-alloc-exp e3))]
    [(GetBang var) (GetBang var)]
    [(SetBang var e) (SetBang var (expose-alloc-exp e))]
    [(Begin es body)
     (Begin (map expose-alloc-exp es) (expose-alloc-exp body))]
    [(WhileLoop cnd body)
     (WhileLoop (expose-alloc-exp cnd) (expose-alloc-exp body))]
    [(Prim op es)
     (Prim op (map expose-alloc-exp es))]))

(define (expose-allocation p)
  (match p
    [(Program info e) (Program info (expose-alloc-exp e))]))