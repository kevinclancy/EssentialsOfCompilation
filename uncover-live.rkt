#lang racket

(require "type-check-Cif.rkt")
(require "interp-Cif.rkt")
(require "utilities.rkt")
(require graph)
(require "multigraph.rkt")
(require "instr-utils.rkt")
(require data/queue)


(provide uncover-live)


(define (analyze-dataflow G transfer bottom join)
  (define mapping (make-hash))
  (for ([v (in-vertices G)])
    (dict-set! mapping v bottom))
  (define worklist (make-queue))
  (for ([v (in-vertices G)])
    (enqueue! worklist v))
  (define trans-G (transpose G))
  (while (not (queue-empty? worklist))
    (define node (dequeue! worklist))
    (define input
      (for/fold ([state bottom])
                ([pred (in-neighbors trans-G node)])
        (join state (dict-ref mapping pred))))
    (define output (transfer node input))
    (cond [(not (equal? output (dict-ref mapping node)))
          (dict-set! mapping node output)
          (for ([v (in-neighbors G node)])
            (enqueue! worklist v))]))
    mapping)
  
(define (build-cfg p)

  (define cfg (make-multigraph '()))
  
  (define (add-edges-from-instr! from-label instr)
    (match instr
      [(Jmp to-label) (add-directed-edge! cfg from-label to-label)]
      [(JmpIf cc to-label) (add-directed-edge! cfg from-label to-label)]
      [else '()]))
  
  (define (add-edges-from-block! block)
    (match block
      [(cons label (Block info instrs))
       (for [(instr instrs)] (add-edges-from-instr! label instr))]))

  (define (add-label! block)
    (match block
      [(cons label (Block info instrs))
       (add-vertex! cfg label)]))

  (match p
    [(X86Program info blocks)
     (for [(b blocks)] (add-label! b))
     (for [(b blocks)] (add-edges-from-block! b))
     (remove-vertex! cfg 'conclusion)
     ;(add-directed-edge! cfg 'start 'conclusion)
     cfg]))

(define (uncover-live p)
  (error "todo: complete this function"))