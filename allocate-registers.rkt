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


(define max-reg-color 10)

; a color corresponding to a register
(struct RegColor (n) #:transparent)

; a color corresponding to the standard data stack
(struct DataStackColor (n) #:transparent)

; a color corresponding to the shadow stack
(struct ShadowStackColor (n) #:transparent)

(define color-to-reg (make-hash
  (list (cons 0 (Reg 'rcx))
        (cons 1 (Reg 'rdx))
        (cons 2 (Reg 'rsi))
        (cons 3 (Reg 'rdi))
        (cons 4 (Reg 'r8))
        (cons 5 (Reg 'r9))
        (cons 6 (Reg 'r10))
        (cons 7 (Reg 'rbx))
        (cons 8 (Reg 'r12))
        (cons 9 (Reg 'r13))
        (cons 10 (Reg 'r14)))))

; returns (var->color, max-data-color, max-shadow-color) triple, where var->color is a dictionary from variables to colors,
; max-data-color is the maximum data color used (or #f if none are used), and max-shadow-color
; is the maximum shadow color used (or #f if none are used)
(define (color-graph conflicts vars vec-vars)
  ; why do we need these?
  ; (define non-alloc-regs (list (Reg 'rax) (Reg 'rsp) (Reg 'rbp) (Reg 'r11) (Reg 'r15)))
  ; (define all-locations (append vars non-alloc-regs))
  (define var->handle (make-hash))
  (define var->saturation (make-hash))    
  (define var->color (make-hash))
  (define max-data-color -1)
  (define max-shadow-color -1)
  
  (define var-pqueue
    (make-pqueue
     (λ (v w)
       (>
        (set-count (dict-ref var->saturation v))
        (set-count (dict-ref var->saturation w))))))
    
  (define (set-loc-color! loc color)
    (match loc
      [(Var x) (dict-set! var->color x color)]
      [else '()])
    (for [(w (in-neighbors conflicts loc))]
      (match w 
        [(Var w)
         (pqueue-decrease-key! var-pqueue (dict-ref var->handle w))
         (set-add! (dict-ref var->saturation w) color)]
        [(Reg r) '()]))) ;; we don't need saturation sets for registers
        

  (define (next-color curr-color var)
    (match curr-color
      [(RegColor n) #:when (< n max-reg-color)
       (RegColor (+ n 1))]
      [(RegColor n) #:when (= n max-reg-color)
       (match (set-member? vec-vars var)
         [#f (DataStackColor 0)]
         [#t (ShadowStackColor 0)])]
      [(DataStackColor n)
       (DataStackColor (+ n 1))]
      [(ShadowStackColor n)
       (ShadowStackColor (+ n 1))]))
  
  (define (get-color var)
    (define sat (dict-ref var->saturation var))
    (define attempt-color (RegColor 0))
    (while (set-member? sat attempt-color)
           (set! attempt-color (next-color attempt-color var)))
    (match attempt-color
      [(DataStackColor n) (set! max-data-color (max max-data-color n))]
      [(ShadowStackColor n) (set! max-shadow-color (max max-shadow-color n))]
      [else '()])
    
    attempt-color)
  
  (for [(v vars)]
    (dict-set! var->saturation v (mutable-set))
    (dict-set! var->handle v (pqueue-push! var-pqueue v)))

  (for ([(color reg) (in-dict color-to-reg)])
    (set-loc-color! reg (RegColor color)))
  
  (while (not (= (pqueue-count var-pqueue) 0))
         (define v (pqueue-pop! var-pqueue))
         (set-loc-color! (Var v) (get-color v)))

  (values var->color max-data-color max-shadow-color))

(define (allocate-registers p)
  (define (is-vector-type ty)
    (match ty
      [`(Vector ,tys ...) #t]
      [else #f]))
  
  (match p
    [(X86Program info blocks)
     (define locals-types (dict-ref info 'locals-types))
     (define vars (sequence->list (in-dict-keys locals-types)))
     (define vec-vars (for/set ([(name ty) (in-dict locals-types)] #:when (is-vector-type ty)) (Var name)))
     (let-values ([(var->color max-data-color max-shadow-color)
                   (color-graph
                    (dict-ref info 'conflicts)
                    vars
                    vec-vars)])
  
       (define (var-to-loc var)
         (match (dict-ref var->color var)
           [(RegColor n) (dict-ref color-to-reg n)]
           [(DataStackColor n) (Deref 'rbp n)]
           [(ShadowStackColor n) (Deref 'r15 n)]))
           
       (define locs (map var-to-loc vars))
       (define callee-save-locs (filter (λ (x) (set-member? callee-save-regs x)) locs))
       (define num-callee-save-locs (length callee-save-locs))
       
       (define (fix-deref loc)
         (match loc
           [(Deref 'rbp i)
            (Deref 'rbp (* -8 (+ num-callee-save-locs i)))]
           [(Deref 'r15 i)
            (Deref 'r15 (* 8 i))]
           [else loc]))
       
       (define (map-arg arg)
         (match arg
           [(Var x) (fix-deref (var-to-loc x))]
           [else arg]))
  
       (define (map-instr instr)
         (match instr
           [(Instr 'sarq (list a1 a2))
            (Instr 'sarq (list (map-arg a1) (map-arg a2)))]
           [(Instr 'andq (list a1 a2))
            (Instr 'andq (list (map-arg a1) (map-arg a2)))]
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

       (define spill-data-space
         (match max-data-color
           [-1 0]
           [n (* n 8)]))

       (define spill-shadow-space
         (match max-shadow-color
           [-1 0]
           [n (* n 8)]))
       
       (define info2 (cons (cons 'spill-space spill-data-space) info))
       (define info3 (cons (cons 'num-root-spills spill-shadow-space) info2))
       (define info4 (cons (cons 'used-callee callee-save-locs) info3))
       (X86Program info4 (map map-block blocks)))
     ]
  ))

