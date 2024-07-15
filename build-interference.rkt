#lang racket

(require "type-check-Cif.rkt")
(require "interp-Cif.rkt")
(require "utilities.rkt")
(require graph)
(require "instr-utils.rkt")

(provide build-interference)

(define (build-interference p)
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
        [else
         (match (list v w)
           [(list (Var x) (Var y))
            (cons (list x y) e)]
           [else e])])))
  
  (define (fold-block block edges)
    (match block
      [(cons label (Block info instrs))
       (define live-sets (dict-ref info 'live-sets))
       (foldl fold-instr edges instrs (rest live-sets))]))

  (match p
    [(X86Program info blocks)
     (define edges (foldl fold-block '() blocks))
     (define conflict-graph (undirected-graph edges))
     (for [(v (in-dict-keys (dict-ref info 'locals-types)))]
       (add-vertex! conflict-graph v))
     (define info2 (dict-set info 'conflicts conflict-graph))
     ; (define info3 (dict-set info2 'conflicts-dbg (graphviz conflict-graph)))
     (X86Program info2 blocks)]))