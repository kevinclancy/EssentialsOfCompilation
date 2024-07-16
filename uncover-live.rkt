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
  (error "todo: complete this function")) ; hint: use multigraph.rkt
  
(define (uncover-live p)
  ; we add a list of live-after sets to each block. Recall that a program contains
  ; one block for each label



  
;;   (define (fold-instr instr live-sets)
;;     (match instr
;;       [(Jmp label)
;;        (cons (dict-ref label->live label) live-sets)]
;;       [(JmpIf cc label)
;;        (define after (set-union (first live-sets) (dict-ref label->live label)))
;;        (cons
;;         (set-union
;;          (set-subtract after (locs-write instr))
;;          (locs-read instr))
;;         live-sets)]
;;       [else 
;;        (define after (first live-sets))
;;        (cons
;;         (set-union
;;          (set-subtract after (locs-write instr))
;;          (locs-read instr))
;;         live-sets)]))

  (match p
    [(X86Program info blocks)
     (define label->live (list (cons 'conclusion (set (Reg 'rax) (Reg 'rsp)))))
       
     (define (fold-instr instr live-sets)
       (define after (first live-sets))
       (cons
        (set-union
         (set-subtract after (locs-write instr))
         (locs-read instr))
        live-sets))
  
     (define (transfer label live-after)
       (define block (dict-ref blocks label)) 
       (match block
         [(Block info instrs)
          (define live-sets
            (foldl fold-instr (list live-after) (reverse instrs)))
          (set! info (dict-set info 'live-sets live-sets))
          (set! blocks (dict-set blocks label (Block info instrs)))
          (first live-sets)]))

     (analyze-dataflow (transpose (build-cfg p)) transfer (set) set-union) 
     
     (X86Program info blocks)]))