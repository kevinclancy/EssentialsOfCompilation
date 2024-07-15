#lang racket
(require racket/set racket/stream)
(require racket/fixnum)
(require graph)
(require "interp.rkt")
(require "interp-Lwhile.rkt")
(require "type-check-Lwhile.rkt")
(require "interp-Cwhile.rkt")
(require "type-check-Cwhile.rkt")
(require "utilities.rkt")
(require "priority_queue.rkt")

;pass modules
(require "shrink.rkt")
(require "uniquify.rkt")
(require "uncover-get.rkt")
(require "remove_complex_operands.rkt")
(require "explicate-control.rkt")
(require "select-instructions.rkt")
(require "uncover-live.rkt")
(require "build-interference.rkt")
(require "allocate-registers.rkt")

(provide (all-defined-out))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lint examples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The following compiler pass is just a silly one that doesn't change
;; anything important, but is nevertheless an example of a pass. It
;; flips the arguments of +. -Jeremy
(define (flip-exp e)
  (match e
    [(Var x) e]
    [(Prim 'read '()) (Prim 'read '())]
    [(Prim '- (list e1)) (Prim '- (list (flip-exp e1)))]
    [(Prim '+ (list e1 e2)) (Prim '+ (list (flip-exp e2) (flip-exp e1)))]))

(define (flip-Lint e)
  (match e
    [(Program info e) (Program info (flip-exp e))]))


;; Next we have the partial evaluation pass described in the book.
(define (pe-neg r)
  (match r
    [(Int n) (Int (fx- 0 n))]
    [else (Prim '- (list r))]))

(define (pe-add r1 r2)
  (match* (r1 r2)
    [((Int n1) (Int n2)) (Int (fx+ n1 n2))]
    [(_ _) (Prim '+ (list r1 r2))]))

(define (pe-exp e)
  (match e
    [(Int n) (Int n)]
    [(Prim 'read '()) (Prim 'read '())]
    [(Prim '- (list e1)) (pe-neg (pe-exp e1))]
    [(Prim '+ (list e1 e2)) (pe-add (pe-exp e1) (pe-exp e2))]))

(define (pe-Lint p)
  (match p
    [(Program info e) (Program info (pe-exp e))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HW1 Passes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (define (uniquify-exp env)
;;   (lambda (e)
;;     (match e
;;       [(Var x)
;;        (Var (dict-ref env x))]
;;       [(Int n) (Int n)]
;;       [(Let x e body)
;;        (define x2 (gensym x))
;;        (define env2 (dict-set env x x2)) 
;;        (Let x2 ((uniquify-exp env) e) ((uniquify-exp env2) body))]
;;       [(Prim op es)
;;        (Prim op (for/list ([e es]) ((uniquify-exp env) e)))])))
;; 
;; ;; uniquify : Lvar -> Lvar
;; (define (uniquify p)
;;   (match p
;;     [(Program info e) (Program info ((uniquify-exp '()) e))]))

;; remove-complex-opera* : Lvar -> Lvar^mon
;; (define (remove-complex-opera* p)
;;   
;;   ;; the argument is already atomic, so we leave it unchanged
;;   (struct atomic-arg (e))
;;   
;;   ;; bind the (non-atomic) argument to a variable so that we can use the (atomic) variable in its place
;;   (struct bind-arg (x e))
;; 
;;   (define (get-arg a)
;;     (match a
;;       [(atomic-arg e) e]
;;       [(bind-arg x e) (Var x)]))
;;   
;;   ; given bindings=[(x1 . e1) (x2 . e2) ... (xn . en)],
;;   ; return an expression (Let x1 e1 (Let x2 e2 ...body)...)
;;   (define (multi-let bindings body)
;;     (match bindings
;;       [(cons (cons x e) rest) (Let x e (multi-let rest body))]
;;       [(list) body]))
;;   
;;   (define (rco_atom e)
;;     (match e
;;       [(Var x) (atomic-arg (Var x))]
;;       [(Int n) (atomic-arg (Int n))]
;;       [(Let x e body)
;;        (define x2 (gensym "x"))
;;        (bind-arg x2 (Let x (rco_exp e) (rco_exp body)))]
;;       [(Prim op es)
;;        (define x2 (gensym "x"))
;;        (define arg_results (for/list ([e es]) (rco_atom e)))
;;        (define args (map get-arg arg_results))
;;        (define bindings
;;          (for/list ([res arg_results] #:when (bind-arg? res))
;;            (cons (bind-arg-x res) (bind-arg-e res))))
;;        (bind-arg x2 (multi-let bindings (Prim op args)))]))
;;   
;;   (define (rco_exp e)
;;     (match e
;;       [(Var x) (Var x)]
;;       [(Int n) (Int n)]
;;       [(Let x e body) (Let x (rco_exp e) (rco_exp body))]
;;       [(Prim op es)
;;        ;todo: is there a standard library "unzip" function? use that here.
;;        (define arg_results (for/list ([e es]) (rco_atom e)))
;;        (define args
;;          (for/list ([res arg_results])
;;            (match res
;;              [(atomic-arg e) e]
;;              [(bind-arg x e) (Var x)])))
;;        (define bindings
;;          (for/list ([res arg_results] #:when (bind-arg? res))
;;            (cons (bind-arg-x res) (bind-arg-e res))))
;;        (multi-let bindings (Prim op args))]))
;; 
;;   (match p
;;     [(Program info e) (Program info (rco_exp e))]))
;;   
;; explicate-control : Lvar^mon -> Cvar
;; (define (explicate-control p)
;; 
;;   (define (explicate_tail e)
;;     (match e
;;       [(Var x) (Return (Var x))]
;;       [(Int n) (Return (Int n))]
;;       [(Let x rhs body) (explicate_assign rhs x (explicate_tail body))]
;;       [(Prim op es)
;;        (define x (gensym "x"))
;;        (Seq (Assign (Var x) (Prim op es)) (Return (Var x)))]
;;       [else (error "explicate_tail unhandled case" e)]))
;;   
;;   (define (explicate_assign e x cont)
;;     (match e
;;       [(Var y) (Seq (Assign (Var x) (Var y)) cont)]
;;       [(Int n) (Seq (Assign (Var x) (Int n)) cont)]
;;       [(Let y rhs body)
;;        (explicate_assign rhs y (explicate_assign body x cont))]
;;       [(Prim op es) (Seq (Assign (Var x) (Prim op es)) cont)]
;;       [else (error "eplicate_assign unhandled case" e)]))
;; 
;;   (match p
;;     [(Program info body)
;;      (define labels `((start . ,(explicate_tail body))))
;;      (CProgram info labels)]))

;; select-instructions : Cvar -> x86var
;; (define (select-instructions p)
;;   (define (atm a)
;;     (match a
;;       [(Var x) (Var x)]
;;       [(Int n) (Imm n)]))
;; 
;;   ; instruction syntax on pg 22
;;   
;;   (define (stmt s) ; returns a list of instructions
;;     (match s
;;       [(Assign (Var x) (Prim '+ (list (Var x) a2)))
;;        (list
;;          (Instr 'addq (list (atm a2) (Var x)))
;;         )
;;        ]
;;       [(Assign (Var x) (Prim '+ (list a1 (Var x))))
;;        (list
;;          (Instr 'addq (list (atm a1) (Var x)))
;;          )
;;        ]
;;       [(Assign (Var x) (Prim '+ (list a1 a2)))
;;        (list
;;          (Instr 'movq (list (atm a1) (Var x)))
;;          (Instr 'addq (list (atm a2) (Var x)))
;;          )
;;        ]
;;       [(Assign (Var x) (Prim '- (list (Var x) a2)))
;;         (list
;;          (Instr 'subq ((atm a2) (Var x)))
;;          )
;;        ]
;;       [(Assign (Var x) (Prim '- (list a1 a2)))
;;         (list
;;          (Instr 'movq (list (atm a1) (Var x)))
;;          (Instr 'subq (list (atm a2) (Var x)))
;;          )
;;        ]
;;       [(Assign (Var x) (Prim '- (list (Var x))))
;;        (list
;;          (Instr 'negq ((Var x)))
;;         )
;;        ]
;;       [(Assign (Var x) (Prim '- (list a)))
;;         (list
;;          (Instr 'movq (list (atm a) (Var x)))
;;          (Instr 'negq (list (Var x)))
;;          )
;;        ]
;;       [(Assign (Var x) (Prim 'read (list)))
;;         (list
;;          (Callq 'read_int 0)
;;          (Instr 'movq (list (Reg 'rax) (Var x)))
;;          )
;;        ]
;;       [(Assign (Var x) (Int n))
;;         (list
;;          (Instr 'movq (list (Imm n) (Var x)))
;;          )
;;        ]
;;       [(Assign (Var x) (Var x))
;;         (list)
;;        ]
;;       [(Assign (Var x) (Var y))
;;         (list
;;          (Instr 'movq (list (Var y) (Var x)))
;;          )
;;        ]
;;       ))
;; 
;;    (define (tail t) ; returns a list of instructions
;;     (match t
;;       [(Return e)
;;         (list
;;          (Instr 'movq (list e (Reg 'rax)))
;;          (Jmp 'conclusion)
;;          )
;;        ]
;;       [(Seq s t)
;;        (append (stmt s) (tail t))]))
;;   
;;   (define (make-block label/tail)
;;     (define l (car label/tail))
;;     (define t (cdr label/tail))
;;     (cons l (Block '() (tail t))))
;;   
;;   (match p
;;     [(CProgram info label/tails)
;;      (X86Program info (map make-block label/tails))]))

;; (define (locs-from-arg arg)
;;   (match arg
;;     [(Imm n) (set)]
;;     [(Var x) (set (Var x))]
;;     [(Reg r) (set (Reg r))]))
;; 
;; (define (locs-read instr)
;;   (match instr
;;     [(Instr 'addq (list a1 a2))
;;      (set-union (locs-from-arg a1) (locs-from-arg a2))]
;;     [(Instr 'subq (list a1 a2))
;;      (set-union (locs-from-arg a1) (locs-from-arg a2))]
;;     [(Instr 'negq (list a))
;;      (locs-from-arg a)]
;;     [(Instr 'movq (list a1 a2))
;;      (locs-from-arg a1)]
;;     [(Instr 'pushq (list a))
;;      (locs-from-arg a)]
;;     [(Instr 'popq (list a))
;;      (set)]
;;     [(Callq label arity)
;;      (match arity
;;        [0 (set)]
;;        [1 (set (Reg 'rdi))]
;;        [2 (set (Reg 'rdi) (Reg 'rsi))]
;;        [3 (set (Reg 'rdi) (Reg 'rsi) (Reg 'rdx))]
;;        [4 (set (Reg 'rdi) (Reg 'rsi) (Reg 'rdx) (Reg 'rcx))]
;;        [5 (set (Reg 'rdi) (Reg 'rsi) (Reg 'rdx) (Reg 'rcx) (Reg 'r8))]
;;        [else (set (Reg 'rdi) (Reg 'rsi) (Reg 'rdx) (Reg 'rcx) (Reg 'r8) (Reg 'r9))])]
;;     [(Retq)
;;      (set)]
;;     [(Jmp label)
;;      (set)]))
;; 
;; (define (locs-write instr)
;;   (match instr
;;     [(Instr 'addq (list a1 a2))
;;      (locs-from-arg a2)]
;;     [(Instr 'subq (list a1 a2))
;;      (locs-from-arg a2)]
;;     [(Instr 'negq (list a))
;;      (locs-from-arg a)]
;;     [(Instr 'movq (list a1 a2))
;;      (locs-from-arg a2)]
;;     [(Instr 'pushq (list a))
;;      (set)]
;;     [(Instr 'popq (list a))
;;      (locs-from-arg a)]
;;     [(Callq label arity)
;;      (set (Reg 'rax) (Reg 'rcx) (Reg 'rdx) (Reg 'rsi) (Reg 'rdi) (Reg 'r8) (Reg 'r9) (Reg 'r10) (Reg 'r11))]
;;     [(Retq)
;;      (set)]
;;     [(Jmp label)
;;      (set)]))

;; (define (build-interference p)
;;   (define (fold-instr instr live-vars edges)
;;     (define write-locations (locs-write instr))
;;     (for*/fold ([e edges] #:result e)
;;               ([v (in-immutable-set live-vars)]
;;                [w (in-immutable-set write-locations)]
;;                #:when (not (equal? v w)))
;;       (match instr
;;         [(Instr 'movq (list a b))
;;          #:when (equal? a v)
;;          e]
;;         [else
;;          (match (list v w)
;;            [(list (Var x) (Var y))
;;             (cons (list x y) e)]
;;            [else e])])))
;;   
;;   (define (fold-block block edges)
;;     (match block
;;       [(cons label (Block info instrs))
;;        (define live-sets (dict-ref info 'live-sets))
;;        (foldl fold-instr edges instrs (rest live-sets))]))
;; 
;;   (match p
;;     [(X86Program info blocks)
;;      (define edges (foldl fold-block '() blocks))
;;      (define conflict-graph (undirected-graph edges))
;;      (for [(v (in-dict-keys (dict-ref info 'locals-types)))]
;;        (add-vertex! conflict-graph v))
;;      (define info2 (dict-set info 'conflicts conflict-graph))
;;      ; (define info3 (dict-set info2 'conflicts-dbg (graphviz conflict-graph)))
;;      (X86Program info2 blocks)]))

;; (define (allocate-registers p)
;; 
;;   ; returns (var->color, max-color) pair, where var->color is a dictionary from variables to colors
;;   ; and max-color is the maximum color used 
;;   (define (color-graph conflicts vars)
;; 
;;     ; why do we need these?
;;     ; (define non-alloc-regs (list (Reg 'rax) (Reg 'rsp) (Reg 'rbp) (Reg 'r11) (Reg 'r15)))
;;     ; (define all-locations (append vars non-alloc-regs))
;;     
;;     (define var->handle (make-hash))
;;     (define var->saturation (make-hash))    
;;     (define var->color (make-hash))
;;     (define max-color 0)
;;     
;;     (define var-pqueue
;;       (make-pqueue
;;        (λ (v w)
;;          (>
;;           (set-count (dict-ref var->saturation v))
;;           (set-count (dict-ref var->saturation w))))))
;;     
;;     (define (set-loc-color! var color)
;;       (dict-set! var->color var color)
;;       (for [(w (in-neighbors conflicts var))]
;;         (cond 
;;           [(dict-has-key? var->handle w)
;;            (pqueue-decrease-key! var-pqueue (dict-ref var->handle w))
;;            (set-add! (dict-ref var->saturation w) color)])))
;; 
;;     (define (get-color var)
;;       (define sat (dict-ref var->saturation var))
;;       (define attempt-color 0)
;;       (while (set-member? sat attempt-color)
;;              (set! attempt-color (+ attempt-color 1)))
;;       (set! max-color (max attempt-color max-color))
;;       attempt-color)
;;     
;;     (for [(v vars)]
;;       (dict-set! var->saturation v (mutable-set))
;;       (dict-set! var->handle v (pqueue-push! var-pqueue v)))
;;     
;;     (while (not (= (pqueue-count var-pqueue) 0))
;;            (define v (pqueue-pop! var-pqueue))
;;            (dict-remove! var->handle v)
;;            (set-loc-color! v (get-color v)))
;; 
;;     (values var->color max-color))
;;   
;;   (match p
;;     [(X86Program info blocks)
;;      (define vars (sequence->list (in-dict-keys (dict-ref info 'locals-types)))) 
;;      (let-values ([(var->color max-color)
;;                    (color-graph
;;                     (dict-ref info 'conflicts)
;;                     vars)])
;;   
;;        (define (var-to-loc var)
;;          (match (dict-ref var->color var)
;;            [0 (Reg 'rcx)]
;;            [1 (Reg 'rdx)]
;;            [2 (Reg 'rsi)]
;;            [3 (Reg 'rdi)]
;;            [4 (Reg 'r8)]
;;            [5 (Reg 'r9)]
;;            [6 (Reg 'r10)]
;;            [7 (Reg 'rbx)]
;;            [8 (Reg 'r12)]
;;            [9 (Reg 'r13)]
;;            [10 (Reg 'r14)]
;;            [else (Deref 'rbp (- (dict-ref var->color var) 10))]))
;; 
;;        (define locs (map var-to-loc vars))
;;        (define callee-save-locs (filter (λ (x) (set-member? callee-save-regs x)) locs))
;;        (define num-callee-save-locs (length callee-save-locs))
;;        
;;        (define (fix-deref loc)
;;          (match loc
;;            [(Deref 'rbp i)
;;             (Deref 'rbp (* -8 (+ num-callee-save-locs i)))]
;;            [else loc]))
;;        
;;        (define (map-arg arg)
;;          (match arg
;;            [(Var x) (fix-deref (var-to-loc x))]
;;            [else arg]))
;;   
;;        (define (map-instr instr)
;;          (match instr
;;            [(Instr 'addq (list a1 a2))
;;             (Instr 'addq (list (map-arg a1) (map-arg a2)))]
;;            [(Instr 'subq (list a1 a2))
;;             (Instr 'subq (list (map-arg a1) (map-arg a2)))]
;;            [(Instr 'negq (list a))
;;             (Instr 'negq (list (map-arg a)))]
;;            [(Instr 'movq (list a1 a2))
;;             (Instr 'movq (list (map-arg a1) (map-arg a2)))]
;;            [(Instr 'pushq (list a))
;;             (Instr 'pushq (list (map-arg a)))]
;;            [(Instr 'popq (list a))
;;             (Instr 'popq (list (map-arg a)))]
;;            [(Callq label arity)
;;             (Callq label arity)]
;;            [(Retq)
;;             (Retq)]
;;            [(Jmp label)
;;             (Jmp label)]))
;;   
;;        (define (map-block block)
;;          (match block
;;            [(cons label (Block info instrs))
;;             (cons label (Block info (map map-instr instrs)))]))
;; 
;;        (define spill-space
;;          (cond 
;;            [(< max-color 11) 0]
;;            [else
;;             (define num-stack-vars (- max-color 10))        
;;             (* num-stack-vars 8)]))
;;        
;;        (define info2 (cons (cons 'spill-space spill-space) info))
;;        (define info3 (cons (cons 'used-callee callee-save-locs) info2))
;;        (X86Program info3 (map map-block blocks)))
;;      ]
;;   ))

;; patch-instructions : x86var -> x86int
(define (patch-instructions p)
  (define (instr1 i)
    (match i
      [(Instr op (list (Imm n) (Deref r m))) #:when (> n 65536)
       (list
        (Instr 'movq (list (Imm n) (Reg 'rax)))
        (Instr op (list (Reg 'rax) (Deref r m))))
       ]
      [(Instr op (list (Deref r m) (Imm n))) #:when (> n 65536)
       (list
        (Instr 'movq (list (Imm n) (Reg 'rax)))
        (Instr op (list (Deref r m) (Reg 'rax))))
       ]
      [else i]))
      
  (define (instr2 i)
    (match i
      [(Instr op (list (Deref r1 n1) (Deref r2 n2)))
       (list
        (Instr 'movq (list (Deref r1 n1) (Reg 'rax)))
        (Instr op (list (Reg 'rax) (Deref r2 n2))))]
      [else i]))

  (define (instr3 i)
    (match i
      [(Instr 'cmpq (list a (Imm n)))
       (list
        (Instr 'movq (list (Imm n) (Reg 'rax)))
        (Instr 'cmpq (list a (Reg 'rax))))]
      [(Instr 'movzbq (list byte-reg (Reg r))) i]
      [(Instr 'movzbq (list byte-reg dest))
       (list
        (Instr 'movzbq (list byte-reg (Reg 'rax)))
        (Instr 'movq (list (Reg 'rax) dest)))]
      [else i]))
  
  (define (block b)
    (match b
      [(cons label (Block info instrs))
       (define instrs1 (flatten (map instr1 instrs)))
       (define instrs2 (flatten (map instr2 instrs1)))
       (define instrs3 (flatten (map instr3 instrs2)))
       (cons label (Block info instrs3))]))
  
  (match p
    [(X86Program info blocks)
     (X86Program info (map block blocks))]))
  
;; prelude-and-conclusion : x86int -> x86int
(define (prelude-and-conclusion p)
  (match p
    [(X86Program info blocks)
     (define used-callee (dict-ref info 'used-callee))
     (define callee-save-space (* 8 (length used-callee)))
     (define spill-space (dict-ref info 'spill-space))
     (define (align x) (if (= (remainder x 16) 0) x (+ x 8)))
     (define all-space (- (align (+ spill-space callee-save-space)) callee-save-space))
     
     (define blocks2
       (dict-set blocks 'main
                 (Block '()
                        (append
                         (list
                          (Instr 'pushq (list (Reg 'rbp)))
                          (Instr 'movq (list (Reg 'rsp) (Reg 'rbp))) )
                         (map (λ (x) (Instr 'pushq (list x))) used-callee)
                         (list
                          (Instr 'subq (list (Imm all-space) (Reg 'rsp)))
                          (Jmp 'start))))))

     (define blocks3
       (dict-set blocks2 'conclusion
                 (Block '()
                        (append
                         (list (Instr 'addq (list (Imm all-space) (Reg 'rsp))))
                         (map (λ (x) (Instr 'popq (list x))) (reverse used-callee))
                         (list
                          (Instr 'popq (list (Reg 'rbp)))
                          (Retq))))))
     
     (X86Program info blocks3)]))

;; Define the compiler passes to be used by interp-tests and the grader
;; Note that your compiler file (the file that defines the passes)
;; must be named "compiler.rkt"
(define compiler-passes
  `(
     ;; Uncomment the following passes as you finish them.
     ("shrink", shrink, interp-Lwhile, type-check-Lwhile)
     ("uniquify" ,uniquify ,interp-Lwhile ,type-check-Lwhile)
     ("uncover-get",uncover-get,interp-Lwhile, type-check-Lwhile)
     ("remove complex opera*" ,remove-complex-operands ,interp-Lwhile ,type-check-Lwhile)
     ("explicate control" ,explicate-control ,interp-Cwhile ,type-check-Cwhile)
     ("instruction selection" ,select-instructions ,interp-pseudo-x86-1)
     ("uncover live", uncover-live, interp-pseudo-x86-1)
     ("build interference", build-interference, interp-pseudo-x86-1)
     ("allocate registers", allocate-registers ,interp-x86-1)
     ("patch instructions" ,patch-instructions ,interp-x86-1)
     ("prelude-and-conclusion" ,prelude-and-conclusion ,interp-x86-1)
     ))
