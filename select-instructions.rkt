#lang racket

(require "type-check-Cif.rkt")
(require "interp-Cif.rkt")
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
      (Instr 'movq (list (Reg 'rax) (Var x)))
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