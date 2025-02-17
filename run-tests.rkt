#! /usr/bin/env racket
#lang racket

(require "utilities.rkt")
(require "interp-Lvec.rkt")
(require "type-check-Lvec.rkt")
(require "interp-Cvec.rkt")
(require "interp.rkt")
(require "compiler.rkt")

(debug-level 1)
(AST-output-syntax 'concrete-syntax)

;; all the files in the tests/ directory with extension ".rkt".
(define all-tests
  (map (lambda (p) (car (string-split (path->string p) ".")))
       (filter (lambda (p)
                 (let ([s (string-split (path->string p) ".")])
                 (and (> (length s) 1) (string=? (cadr (string-split (path->string p) ".")) "rkt"))))
               (directory-list (build-path (current-directory) "tests")))))

(define (tests-for r)
  (map (lambda (p)
         (caddr (string-split p "_")))
       (filter
        (lambda (p)
          (string=? r (car (string-split p "_"))))
        all-tests)))

;; The following tests the intermediate-language outputs of the passes.
(interp-tests "vectors" type-check-Lvec compiler-passes #f "vectors_test" (tests-for "vectors"))
;(interp-tests "var" #f compiler-passes interp-Lvar "var_test" (tests-for "var"))
;(interp-tests "cond" type-check-Lif compiler-passes #f "cond_test" (tests-for "cond"))

;; The following tests the final x86 code.
;(compiler-tests "cond" type-check-Lif compiler-passes "cond_test" (tests-for "cond"))
;(compiler-tests "cond" type-check-Lwhile compiler-passes "while_test" (tests-for "while"))