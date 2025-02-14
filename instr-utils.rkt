#lang racket

(require "type-check-Cif.rkt")
(require "interp-Cif.rkt")
(require "utilities.rkt")

(provide locs-read locs-write)

(define (locs-from-arg arg)
  (match arg
    [(Global name) (set)]
    [(Imm n) (set)]
    [(Var x) (set (Var x))]
    [(Reg r) (set (Reg r))]
    [(Deref reg i) (set)]
    [(ByteReg r) (set (ByteReg r))]))

(define (locs-read instr)
  (match instr
    [(Instr 'andq (list a1 a2))
     (set-union (locs-from-arg a1) (locs-from-arg a2))]
    [(Instr 'sarq (list a1 a2))
     (set-union (locs-from-arg a1) (locs-from-arg a2))]
    [(Instr 'xorq (list a1 a2))
     (set-union (locs-from-arg a1) (locs-from-arg a2))]
    [(Instr 'cmpq (list a1 a2))
     (set-union (locs-from-arg a1) (locs-from-arg a2))]
    [(Instr 'set (list cc a))
     (set)]
    [(Instr 'movzbq (list a1 a2))
     (set (locs-from-arg a1))]
    [(Instr 'addq (list a1 a2))
     (set-union (locs-from-arg a1) (locs-from-arg a2))]
    [(Instr 'subq (list a1 a2))
     (set-union (locs-from-arg a1) (locs-from-arg a2))]
    [(Instr 'negq (list a))
     (locs-from-arg a)]
    [(Instr 'movq (list a1 a2))
     (locs-from-arg a1)]
    [(Instr 'pushq (list a))
     (locs-from-arg a)]
    [(Instr 'popq (list a))
     (set)]
    [(Callq label arity)
     (match arity
       [0 (set)]
       [1 (set (Reg 'rdi))]
       [2 (set (Reg 'rdi) (Reg 'rsi))]
       [3 (set (Reg 'rdi) (Reg 'rsi) (Reg 'rdx))]
       [4 (set (Reg 'rdi) (Reg 'rsi) (Reg 'rdx) (Reg 'rcx))]
       [5 (set (Reg 'rdi) (Reg 'rsi) (Reg 'rdx) (Reg 'rcx) (Reg 'r8))]
       [else (set (Reg 'rdi) (Reg 'rsi) (Reg 'rdx) (Reg 'rcx) (Reg 'r8) (Reg 'r9))])]
    [(Retq)
     (set (Reg 'rax))]
    [(Jmp label)
     (set)]
    [(JmpIf cc label) ; i don't need to include the eflags register, do I?
     (set)]))

(define (locs-write instr)
  (match instr
    [(Instr 'andq (list a1 a2))
     (locs-from-arg a2)]
    [(Instr 'sarq (list a1 a2))
     (locs-from-arg a2)]
    [(Instr 'xorq (list a1 a2))
     (locs-from-arg a2)]
    [(Instr 'cmpq (list a1 a2))
     (set)] ; we don't need to include the eflags register in this set, do we?
    [(Instr 'set (list cc a))
     (locs-from-arg a)]
    [(Instr 'movzbq (list a1 a2))
     (locs-from-arg a2)]
    [(Instr 'addq (list a1 a2))
     (locs-from-arg a2)]
    [(Instr 'subq (list a1 a2))
     (locs-from-arg a2)]
    [(Instr 'negq (list a))
     (locs-from-arg a)]
    [(Instr 'movq (list a1 a2))
     (locs-from-arg a2)]
    [(Instr 'pushq (list a))
     (set)]
    [(Instr 'popq (list a))
     (locs-from-arg a)]
    [(Callq label arity)
     (set (Reg 'rax) (Reg 'rcx) (Reg 'rdx) (Reg 'rsi) (Reg 'rdi) (Reg 'r8) (Reg 'r9) (Reg 'r10) (Reg 'r11))]
    [(Retq)
     (set)]
    [(Jmp label)
     (set)]
    [(JmpIf cc label)
     (set)]))