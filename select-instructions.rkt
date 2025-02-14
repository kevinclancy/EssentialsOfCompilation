#lang racket

(require "type-check-Cvec.rkt")
(require "interp-Cvec.rkt")
(require "utilities.rkt")

(provide select-instructions)

(define (atm a)
  (match a
    [(Var x) (Var x)]
    [(Int n) (Imm n)]
    [(Void) (Imm 0)]
    [(Bool #t) (Imm 1)]
    [(Bool #f) (Imm 0)]))

(define prim->cc '((eq? . e) (< . l) (<= . le) (> . g) (>= . ge)))

; instruction syntax on pg 22  
(define (stmt s) ; returns a list of instructions
  (match s
    [(Assign (Var x) (Prim 'vector-ref (list tup (Int n))))
     (list
      (Instr 'movq (list (atm tup) (Reg 'r11)))
      (Instr 'movq (list (Deref 'r11 (* 8 (+ n 1))) (Var x))))]
    [(Assign (Var x) (Prim 'vector-set! (list tup (Int n) rhs)))
     (list
      (Instr 'movq (list (atm tup) (Reg 'r11)))
      (Instr 'movq (list (atm rhs) (Deref 'r11 (* 8 (+ n 1)))))
      (Instr 'movq (list (Imm 0) (Var x))))]
    [(Prim 'vector-set! (list tup (Int n) rhs))
     (list
      (Instr 'movq (list (atm tup) (Reg 'r11)))
      (Instr 'movq (list (atm rhs) (Deref 'r11 (* 8 (+ n 1))))))]
    [(Assign (Var x) (Prim 'vector-length (list tup)))
     (list
      (Instr 'movq (list (atm tup) (Reg 'r11)))
      (Instr 'movq (list (Deref 'r11 0) (Reg 'rax)))
      (Instr 'andq (list (Imm #x00000000000000FF) (Reg 'rax)))
      (Instr 'sarq (list (Imm 1) (Reg 'rax)))
      (Instr 'movq (list (Reg 'rax) (Var x))))]
    [(Assign (Var x) (GlobalValue name))
     (list
      (Instr 'movq (list (Global name) (Var x))))]
    [(Assign (Var x) (Allocate n `(Vector ,tys ...)))
     (define is-vector-field
       (for/fold
           ([sum 0])
           ([i (range (length tys))] [ty tys])
           (match ty
             [`(Vector ,tys^ ...) (+ sum (arithmetic-shift 1 i))]
             [else sum])))
     (define is-vector-field^ (arithmetic-shift is-vector-field 8))
     (define vector-length-field (arithmetic-shift n 1)) 
     (define forward-bit-field 1)
     (define tag (bitwise-ior is-vector-field^ vector-length-field forward-bit-field))
     (list
      (Instr 'movq (list (Global 'free_ptr) (Reg 'r11)))
      (Instr 'addq (list (Imm (* 8 (+ n 1))) (Global 'free_ptr)))
      (Instr 'movq (list (Imm tag) (Deref 'r11 0)))
      (Instr 'movq (list (Reg 'r11) (Var x))))]
    [(Collect n)
     (list
      (Instr 'movq (list (Reg 'r15) (Reg 'rdi)))
      (Instr 'movq (list (Imm n) (Reg 'rsi)))
      (Callq 'collect 2))]
    [(Assign (Var x) (Prim 'not (list (Var x))))
     (list
      (Instr 'xorq (list (Imm 1) (Var x))))]
    [(Assign (Var x) (Prim 'not (list a)))
     (list
      (Instr 'movq (list (atm a) (Var x)))
      (Instr 'negq (list (Imm 1) (Var x))))]
    [(Assign (Var x) (Prim 'eq? (list a1 a2)))
     (list
      (Instr 'cmpq (list (atm a2) (atm a1)))
      (Instr 'set (list 'e (ByteReg 'al)))
      (Instr 'movzbq (list (ByteReg 'al) (Var x))))]
    ; eq? < <= > >=
    [(Assign (Var x) (Prim '< (list a1 a2)))
     (list
      (Instr 'cmpq (atm a2) (atm a1))
      (Instr 'set (list 'l (ByteReg 'al)))
      (Instr 'movzbq (list ByteReg 'al) (Var x)))]
    [(Assign (Var x) (Prim '<= (list a1 a2)))
     (list
      (Instr 'cmpq (atm a2) (atm a1))
      (Instr 'set (list 'le (ByteReg 'al)))
      (Instr 'movzbq (list ByteReg 'al) (Var x)))]
    [(Assign (Var x) (Prim '> (list a1 a2)))
     (list
      (Instr 'cmpq (atm a2) (atm a1))
      (Instr 'set (list 'g (ByteReg 'al)))
      (Instr 'movzbq (list ByteReg 'al) (Var x)))]
    [(Assign (Var x) (Prim '>= (list a1 a2)))
     (list
      (Instr 'cmpq (atm a2) (atm a1))
      (Instr 'set (list 'ge (ByteReg 'al)))
      (Instr 'movzbq (list ByteReg 'al) (Var x)))]
    [(Assign (Var x) (Prim '+ (list (Var x) a2)))
     (list
      (Instr 'addq (list (atm a2) (Var x)))
      )
     ]
    [(Assign (Var x) (Prim '+ (list a1 (Var x))))
     (list
      (Instr 'addq (list (atm a1) (Var x)))
      )
     ]
    [(Assign (Var x) (Prim '+ (list a1 a2)))
     (list
      (Instr 'movq (list (atm a1) (Var x)))
      (Instr 'addq (list (atm a2) (Var x)))
      )
     ]
    [(Assign (Var x) (Prim '- (list (Var x) a2)))
     (list
      (Instr 'subq ((atm a2) (Var x)))
      )
     ]
    [(Assign (Var x) (Prim '- (list a1 a2)))
     (list
      (Instr 'movq (list (atm a1) (Var x)))
      (Instr 'subq (list (atm a2) (Var x)))
      )
     ]
    [(Assign (Var x) (Prim '- (list (Var x))))
     (list
      (Instr 'negq ((Var x)))
      )
     ]
    [(Assign (Var x) (Prim '- (list a)))
     (list
      (Instr 'movq (list (atm a) (Var x)))
      (Instr 'negq (list (Var x)))
      )
     ]
    [(Assign (Var x) (Prim 'read (list)))
     (list
      (Callq 'read_int 0)
      (Instr 'movq (list 'rax) (Var x))
      )
     ]
    [(Assign (Var x) (Int n))
     (list
      (Instr 'movq (list (Imm n) (Var x)))
      )
     ]
    [(Assign (Var x) (Bool b))
     (list
      (Instr 'movq (list (Imm (if b 1 0)) (Var x))))]
    [(Assign (Var x) (Void))
     (list
      (Instr 'movq (list (Imm 0) (Var x))))]
    [(Assign (Var x) (Var x))
     (list)
     ]
    [(Assign (Var x) (Var y))
     (list
      (Instr 'movq (list (Var y) (Var x)))
      )
     ]
    ))

(define (tail t) ; returns a list of instructions
  (match t
    [(Goto l)
     (list
      (Jmp l))]
    [(IfStmt (Prim cmp (list a1 a2)) (Goto l1) (Goto l2)) #:when (member cmp (list 'eq? '< '<= '> '>=))
     (list
      (Instr 'cmpq (list (atm a2) (atm a1)))
      (JmpIf (dict-ref prim->cc cmp) l1)
      (Jmp l2))]
    [(Return a)
     (list
      (Instr 'movq (list (atm a) (Reg 'rax)))
      (Jmp 'conclusion)
      )
     ]
    [(Seq s t)
     (append (stmt s) (tail t))]))

(define (make-block label/tail)
  (define l (car label/tail))
  (define t (cdr label/tail))
  (cons l (Block '() (tail t))))

(define (select-instructions p)
    (match p
    [(CProgram info label/tails)
     (X86Program info (map make-block label/tails))]))