#lang racket

(require "type-check-Cif.rkt")
(require "interp-Cif.rkt")
(require "utilities.rkt")
(require graph)
(require "instr-utils.rkt")
(require "priority_queue.rkt")

(provide allocate-registers)

(define callee-save-regs
  (set
   (Reg 'rsp) (Reg 'rbp) (Reg 'rbx) (Reg 'r12)
   (Reg 'r13) (Reg 'r14) (Reg 'r15)))

; returns (var->color, max-color) pair, where var->color is a dictionary from variables to colors
; and max-color is the maximum color used 
(define (color-graph conflicts vars)
  ; why do we need these?
  ; (define non-alloc-regs (list (Reg 'rax) (Reg 'rsp) (Reg 'rbp) (Reg 'r11) (Reg 'r15)))
  ; (define all-locations (append vars non-alloc-regs))
  (define var->handle (make-hash))
  (define var->saturation (make-hash))    
  (define var->color (make-hash))
  (define max-color 0)
    
  (define var-pqueue
    (make-pqueue
     (λ (v w)
       (>
        (set-count (dict-ref var->saturation v))
        (set-count (dict-ref var->saturation w))))))
    
  (define (set-loc-color! var color)
    (dict-set! var->color var color)
    (for [(w (in-neighbors conflicts var))]
      (cond 
        [(dict-has-key? var->handle w)
         (pqueue-decrease-key! var-pqueue (dict-ref var->handle w))
         (set-add! (dict-ref var->saturation w) color)])))

  (define (get-color var)
    (define sat (dict-ref var->saturation var))
    (define attempt-color 0)
    (while (set-member? sat attempt-color)
           (set! attempt-color (+ attempt-color 1)))
    (set! max-color (max attempt-color max-color))
    attempt-color)
    
  (for [(v vars)]
    (dict-set! var->saturation v (mutable-set))
    (dict-set! var->handle v (pqueue-push! var-pqueue v)))
    
  (while (not (= (pqueue-count var-pqueue) 0))
         (define v (pqueue-pop! var-pqueue))
         (dict-remove! var->handle v)
         (set-loc-color! v (get-color v)))

  (values var->color max-color))

(define (allocate-registers p)
  (match p
    [(X86Program info blocks)
     (define vars (sequence->list (in-dict-keys (dict-ref info 'locals-types)))) 
     (let-values ([(var->color max-color)
                   (color-graph
                    (dict-ref info 'conflicts)
                    vars)])
  
       (define (var-to-loc var)
         (match (dict-ref var->color var)
           [0 (Reg 'rcx)]
           [1 (Reg 'rdx)]
           [2 (Reg 'rsi)]
           [3 (Reg 'rdi)]
           [4 (Reg 'r8)]
           [5 (Reg 'r9)]
           [6 (Reg 'r10)]
           [7 (Reg 'rbx)]
           [8 (Reg 'r12)]
           [9 (Reg 'r13)]
           [10 (Reg 'r14)]
           [else (Deref 'rbp (- (dict-ref var->color var) 10))]))

       (define locs (map var-to-loc vars))
       (define callee-save-locs (filter (λ (x) (set-member? callee-save-regs x)) locs))
       (define num-callee-save-locs (length callee-save-locs))
       
       (define (fix-deref loc)
         (match loc
           [(Deref 'rbp i)
            (Deref 'rbp (* -8 (+ num-callee-save-locs i)))]
           [else loc]))
       
       (define (map-arg arg)
         (match arg
           [(Var x) (fix-deref (var-to-loc x))]
           [else arg]))
  
       (define (map-instr instr)
         (match instr
           [(Instr 'cmpq (list a1 a2))
            (Instr 'cmpq (list (map-arg a1) (map-arg a2)))]
           [(Instr 'xorq (list a1 a2))
            (Instr 'xorq (list (map-arg a1) (map-arg a2)))]
           [(Instr 'set (list cc a))
            (Instr 'set (list cc (map-arg a)))]
           [(Instr 'movzbq (list byte-reg a))
            (Instr 'movzbq (list byte-reg (map-arg a)))]
           [(Instr 'addq (list a1 a2))
            (Instr 'addq (list (map-arg a1) (map-arg a2)))]
           [(Instr 'subq (list a1 a2))
            (Instr 'subq (list (map-arg a1) (map-arg a2)))]
           [(Instr 'negq (list a))
            (Instr 'negq (list (map-arg a)))]
           [(Instr 'movq (list a1 a2))
            (Instr 'movq (list (map-arg a1) (map-arg a2)))]
           [(Instr 'pushq (list a))
            (Instr 'pushq (list (map-arg a)))]
           [(Instr 'popq (list a))
            (Instr 'popq (list (map-arg a)))]
           [(Callq label arity)
            (Callq label arity)]
           [(Retq)
            (Retq)]
           [(JmpIf cc label)
            (JmpIf cc label)]
           [(Jmp label)
            (Jmp label)]))
  
       (define (map-block block)
         (match block
           [(cons label (Block info instrs))
            (cons label (Block info (map map-instr instrs)))]))

       (define spill-space
         (cond 
           [(< max-color 11) 0]
           [else
            (define num-stack-vars (- max-color 10))        
            (* num-stack-vars 8)]))
       
       (define info2 (cons (cons 'spill-space spill-space) info))
       (define info3 (cons (cons 'used-callee callee-save-locs) info2))
       (X86Program info3 (map map-block blocks)))
     ]
  ))

