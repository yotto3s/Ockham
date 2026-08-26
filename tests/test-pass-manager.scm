#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*-
#!r6rs

(import (rnrs (6))
        (srfi :64 testing)
        (ockham core)
        (ockham ops)
        (ockham pass-manager))

(test-begin "ockham-pass-manager")

(test-group "pass-construction"
  (let* ((p1 (make-pass 'pass1 (lambda (m) m)))
         (p2 (make-pass 'pass2 (lambda (m) m) "A test pass")))
    (test-assert (pass? p1))
    (test-equal 'pass1 (pass-name p1))
    (test-equal "" (pass-description p1))
    (test-assert (procedure? (pass-proc p1)))

    (test-assert (pass? p2))
    (test-equal 'pass2 (pass-name p2))
    (test-equal "A test pass" (pass-description p2))))

(define-pass test-pass-macro (m)
  "Macro defined pass"
  m)

(test-group "define-pass-macro"
  (test-assert (pass? test-pass-macro))
  (test-equal 'test-pass-macro (pass-name test-pass-macro))
  (test-equal "Macro defined pass" (pass-description test-pass-macro)))

(test-group "pass-manager-execution"
  (let* ((pm (make-pass-manager))
         (counter 0)
         (p1 (make-pass 'inc1 (lambda (m) (set! counter (+ counter 1)) m)))
         (p2 (make-pass 'inc2 (lambda (m) (set! counter (+ counter 10)) m)))
         (blk (make-block '^bb0 '()))
         (reg (make-region (list blk)))
         (mod (make-module '$test_mod reg)))
    (test-assert (pass-manager? pm))
    (test-equal '() (pass-manager-passes pm))

    (pass-manager-add-pass! pm p1)
    (pass-manager-add-pass! pm p2)
    (test-equal 2 (length (pass-manager-passes pm)))

    (let ((result (pass-manager-run pm mod)))
      (test-assert (module? result))
      (test-equal '$test_mod (module-name result))
      (test-equal 11 counter))))

(test-group "traversal-helpers"
  (let* ((inst1 (read-instruction '(%x :i32 = (constant :i32 1))))
         (inst2 (read-instruction '(%y :i32 = (constant :i32 2))))
         (blk (make-block '^bb0 (list inst1 inst2)))
         (reg (make-region (list blk)))
         (mod (make-module '$test_mod reg))

         ;; Map instructions: count total instructions
         (count 0)
         (transformed-mod (module-map-instructions
                            (lambda (inst)
                              (set! count (+ count 1))
                              inst)
                            mod)))
    (test-equal 2 count)
    (test-assert (module? transformed-mod))
    (test-equal '$test_mod (module-name transformed-mod))))

(test-end "ockham-pass-manager")
