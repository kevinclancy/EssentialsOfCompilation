#lang racket
(require graph)
(require "multigraph.rkt")
(require "utilities.rkt")
(require "type-check-Rwhile.rkt")
(require "type-check-Cvar.rkt")
(require "type-check-Cif.rkt")
(provide type-check-Cwhile type-check-Cwhile-mixin type-check-Cwhile-class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type-check-Cwhile

;; Dataflow analysis

(define (type-check-Cwhile-mixin super-class)
  (class super-class
    (super-new)
    (inherit type-check-exp check-type-equal? type-equal?)
    ; type-check-apply fun-def-type

    (define/public (combine-types t1 t2)
      (match (list t1 t2)
        [(list '_ t2) t2]
        [(list t1 '_) t1]
        [else
         t1]))
    
    (define/public (free-vars-exp e)
      (define (recur e) (send this free-vars-exp e))
      (match e
        [(Var x) (set x)]
        [(HasType e t) (recur e)]
        [(Int n) (set)]
        [(Bool b) (set)]
        [(FunRef f) (set)]
        [(FunRefArity f n) (set)]
        [(Let x e body)
	 (set-union (recur e) (set-remove (recur body) x))]
        [(If cnd thn els)
         (set-union (recur cnd) (recur thn) (recur els))]
	[(Lambda (list `[,xs : ,Ts] ...) rT body)
         (define (rm x s) (set-remove s x))
         (foldl rm (recur body) xs)]
	[(Apply e es)
	 (apply set-union (cons (recur e) (map recur es)))]
	[(Prim op es)
	 (apply set-union (cons (set) (map recur es)))]
        [(WhileLoop cnd body)
         (set-union (recur cnd) (recur body))]
        [(Begin es e)
         (apply set-union (cons (recur e) (map recur es)))]
        [(SetBang x rhs) (set-union (set x) (recur rhs))]
        [(ValueOf e ty) (recur e)]
        [(Exit) (set)]
        ;; C-level expressions
        [(Void) (set)]
        [(Allocate size ty) (set)]
        [(AllocateClosure len ty arity) (set)]
        [(GlobalValue name) (set)]
        [(Call f arg*) (apply set-union (cons (recur f) (map recur arg*)))]
	[else (error 'free-vars-exp "unmatched ~a" e)]))
    
    (define type-changed #t)

    (define (exp-ready? exp env)
      (for/and ([x (free-vars-exp exp)])
        (dict-has-key? env x)))

    (define (update-type x t env)
      (cond [(dict-has-key? env x)
             (define old-t (dict-ref env x))
             (unless (type-equal? t old-t)
               (error 'update-type "old type ~a and new type ~ are inconsistent"
                      old-t t))
             (define new-t (combine-types old-t t))
             (cond [(not (equal? new-t old-t))
                    (dict-set! env x new-t)
                    (set! type-changed #t)])]
            [(eq? t '_)
             (void)]
            [else
             (set! type-changed #t)
             (dict-set! env x t)]))

    (define/override (type-check-stmt env)
      (lambda (s)
        (verbose 'type-check-stmt s)
        (match s
          [(Assign (Var x) e)
           #:when (exp-ready? e env)
           (define-values (e^ t) ((type-check-exp env) e))
           (update-type x t env)]
          [(Assign (Var x) e) (void)]
          [(Collect size) (void)]
          [(Exit) (void)]
          [(Prim 'vector-set! (list vec index rhs))
           #:when (and (exp-ready? vec env) (exp-ready? index env)
                       (exp-ready? rhs env))
           ((type-check-exp env) s)]
          [(Prim 'read '()) (void)]
          #;[(Call e es)
           #:when (and (exp-ready? e env)
                       (for/and ([arg es]) (exp-ready? arg env)))
           (define-values (e^ es^ rt) (type-check-apply env e es))
           (void)]
          [else (void)]
          )))

    (define/override (type-check-tail env block-env blocks)
      (lambda (t)
        (verbose 'type-check-tail t)
        (match t
          [(Return e)
           #:when (exp-ready? e env)
           (define-values (e^ t) ((type-check-exp env) e))
           t]
          [(Return e) '_]      
          [(Seq s t)
           ((type-check-stmt env) s)
           ((type-check-tail env block-env blocks) t)]
          [(Goto label)
           (cond [(dict-has-key? block-env label)
                  (dict-ref block-env label)]
                 [else '_])]
          [(IfStmt cnd tail1 tail2)
           (cond [(exp-ready? cnd env)
                  (define-values (c Tc) ((type-check-exp env) cnd))
                  (unless (type-equal? Tc 'Boolean)
                    (error "type error: condition should be Boolean, not" Tc))
                  ])
           (define T1 ((type-check-tail env block-env blocks) tail1))
           (define T2 ((type-check-tail env block-env blocks) tail2))
           (unless (type-equal? T1 T2)
             (error "type error: branches of if should have same type, not"
                    T1 T2))
           (combine-types T1 T2)]
          #;[(TailCall f arg*)
           #:when (and (exp-ready? f env)
                       (for/and ([arg arg*]) (exp-ready? arg env)))
           (define-values (f^ arg*^ rt) (type-check-apply env f arg*))
           rt]
          #;[(TailCall f arg*) '_]
          [(Exit) '_]
          )))

    (define (adjacent-tail t)
      (match t
        [(Goto label) (set label)]
        [(IfStmt cnd t1 t2) (set-union (adjacent-tail t1) (adjacent-tail t2))]
        [(Seq s t) (adjacent-tail t)]
        [else (set)]))

    (define (C-blocks->CFG blocks)
      (define G (make-multigraph '()))
      (for ([label (in-dict-keys blocks)])
        (add-vertex! G label))
      (for ([(src b) (in-dict blocks)])
        (for ([tgt (adjacent-tail b)])
          (add-directed-edge! G src tgt)))
      G)

    #;(define/override (type-check-def global-env)
      (lambda (d)
        (match d
          [(Def f (and p:t* (list `[,xs : ,ps] ...)) rt info blocks)
           (verbose "type-check-def" f)
           (define env (make-hash (append (map cons xs ps) global-env)))
           (define block-env (make-hash))
           (set! type-changed #t)
           (define (iterate)
             (cond [type-changed
                    (set! type-changed #f)
                    (for ([(label tail) (in-dict blocks)])
                      (define t ((type-check-tail env block-env blocks) tail))
                      (update-type label t block-env)
                      )
                    (verbose "type-check-def" env block-env)
                    (iterate)]
                   [else (void)]))
           (iterate)
           (define start (symbol-append f 'start))
           (unless (dict-has-key? block-env start)
             (error 'type-check-def "failed to infer type for ~a" start))
           (define t (dict-ref block-env start))
           (unless (type-equal? t rt)
             (error "mismatching return type" t rt))
           (define locals-types
             (for/list ([(x t) (in-dict env)]
                        #:when (not (dict-has-key? global-env x)))
               (cons x t)))
           (define new-info (dict-set info 'locals-types locals-types))
           (Def f p:t* rt new-info blocks)]
          )))

    #;(define/override (type-check-program p)
      (match p
        [(ProgramDefs info ds)
         (define new-env (for/list ([d ds]) 
                           (cons (Def-name d) (fun-def-type d))))
         (define ds^ (for/list ([d ds])
                       ((type-check-def new-env) d)))
         (ProgramDefs info ds^)]))

    (define/override (type-check-program p)
      (match p
        [(CProgram info blocks)
         (define env (make-hash))
         (define block-env (make-hash))
         (set! type-changed #t)
         (define (iterate)
           (cond [type-changed
                  (set! type-changed #f)
                  (for ([(label tail) (in-dict blocks)])
                    (define t ((type-check-tail env block-env blocks) tail))
                    (update-type label t block-env)
                    )
                  (verbose "type-check-def" env block-env)
                  (iterate)]
                 [else (void)]))
         (iterate)
         
         (define start 'start)
         (unless (dict-has-key? block-env start)
           (error 'type-check-def "failed to infer type for ~a" start))
         (define t (dict-ref block-env start))
         (define locals-types
           (for/list ([(x t) (in-dict env)])
             (cons x t)))
         (define new-info (dict-set info 'locals-types locals-types))
         (unless (type-equal? t 'Integer)
           (error "return type of program must be Integer, not" t))
         (CProgram new-info blocks)]
        [else (super type-check-program p)]))

    ))

(define type-check-Cwhile-class (type-check-Cwhile-mixin
                                 (type-check-Cif-mixin
                                  (type-check-Cvar-mixin
                                   type-check-Rwhile-class))))

(define (type-check-Cwhile p)
  (send (new type-check-Cwhile-class) type-check-program p))
  