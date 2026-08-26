#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*-
#!r6rs

(import (rnrs (6))
        (srfi :64 testing)
        (ockham core)
        (ockham ops)
        (ockham pass-manager)
        (ockham wasm-emit))

(test-begin "ockham-wasm-emit")

(test-group "func-and-arithmetic-wasm-emission"
  (let* ((inst-add (read-instruction '(%c :i32 = (add :i32 %a %b))))
         (inst-ret (read-instruction '(ret %c)))
         (func-blk (make-block '^bb0 (list inst-add inst-ret)))
         (func-reg (make-region (list func-blk)))
         (func-op (make-func '$add (list (cons '%a (make-i32)) (cons '%b (make-i32))) (list (make-i32)) func-reg))
         (mod-inst (make-instruction 'func func-op #f '()))
         (mod-blk (make-block '^bb0 (list mod-inst)))
         (mod-reg (make-region (list mod-blk)))
         (mod (make-module '$math mod-reg))

         ;; Emit WAT
         (wat (emit-wasm-wat mod)))

    (test-assert (list? wat))
    (test-equal 'module (car wat))
    (test-equal '$math (cadr wat))

    (let ((func-sexp (caddr wat)))
      (test-equal 'func (car func-sexp))
      (test-equal '$add (cadr func-sexp))
      (test-equal '(export "add") (caddr func-sexp))
      (test-equal '(param $a i32) (cadddr func-sexp))
      (test-equal '(param $b i32) (car (cddddr func-sexp)))
      (test-equal '(result i32) (cadr (cddddr func-sexp))))))

(test-group "wasm-emit-pass-integration"
  (let* ((inst-c (read-instruction '(%x :i32 = (constant :i32 42))))
         (inst-r (read-instruction '(ret %x)))
         (func-blk (make-block '^bb0 (list inst-c inst-r)))
         (func-reg (make-region (list func-blk)))
         (func-op (make-func '$const_42 '() (list (make-i32)) func-reg))
         (mod-inst (make-instruction 'func func-op #f '()))
         (mod-blk (make-block '^bb0 (list mod-inst)))
         (mod-reg (make-region (list mod-blk)))
         (mod (make-module '$const_mod mod-reg))

         ;; Pass Manager Execution
         (pm (make-pass-manager))
         (_ (pass-manager-add-pass! pm wasm-emit-pass))
         (result (pass-manager-run pm mod)))

    (test-assert (list? result))
    (test-equal 'module (car result))
    (test-equal '$const_mod (cadr result))))

(test-end "ockham-wasm-emit")
