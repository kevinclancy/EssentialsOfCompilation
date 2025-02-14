#lang racket

(require "type-check-Cif.rkt")
(require "interp-Cif.rkt")
(require "utilities.rkt")
(require graph)
(require "instr-utils.rkt")

(provide build-interference)

(define (build-interference p)
  (define (fold-instr-collect vec-vars instr live-vars edges)
    (match instr
      [(Callq 'collect 2)
       (define live-vec-vars (set-intersect vec-vars live-vars))
       (for*/fold ([e edges] #:result e)
                  ([v (in-immutable-set live-vec-vars)]
                   [w (in-immutable-set callee-save)])
                   (cons (list v (Reg w)) e))]
      [else edges]))

  (define (fold-instr instr live-vars edges)
    (define write-locations (locs-write instr))
    (for*/fold ([e edges] #:result e)
              ([v (in-immutable-set live-vars)]
               [w (in-immutable-set write-locations)]
               #:when (not (equal? v w)))
      (match instr
        [(Instr 'movq (list a b))
         #:when (equal? a v)
         e]
        [else (cons (list v w) e)])))

  (define (fold-block vec-vars block edges)
    (match block
      [(cons label (Block info instrs))
       (define live-sets (dict-ref info 'live-sets))
       (define edges^ (foldl fold-instr edges instrs (rest live-sets)))
       (foldl (λ (instr live-vars edges) (fold-instr-collect vec-vars instr live-vars edges))
              edges^
              instrs
              (rest live-sets))]))

  (define (is-vector-type ty)
    (match ty
      [`(Vector ,tys ...) #t]
      [else #f]))

  (match p
    [(X86Program info blocks)
     (define locals-types (dict-ref info 'locals-types))
     (define vec-vars (for/set ([(name ty) (in-dict locals-types)] #:when (is-vector-type ty)) (Var name)))
     (define edges (foldl (λ (block edges) (fold-block vec-vars block edges)) '() blocks))
     (define conflict-graph (undirected-graph edges))
     (for [(v (in-dict-keys (dict-ref info 'locals-types)))]
       (add-vertex! conflict-graph (Var v)))
     (for [(r (in-set (set-union caller-save callee-save)))]
       (add-vertex! conflict-graph (Reg r)))
     (define info2 (dict-set info 'conflicts conflict-graph))
     (X86Program info2 blocks)]))