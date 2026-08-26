#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*-
#!r6rs

(import (rnrs (6))
        (srfi :64 testing)
        (ockham core)
        (ockham ops)
        (ockham pass-manager)
        (ockham wasm-binary-emit))

(test-begin "ockham-wasm-binary-emit")

(test-group "binary-wasm-emission-and-execution"
  (let* ((inst-add (read-instruction '(%c :i32 = (add :i32 %a %b))))
         (inst-ret (read-instruction '(ret %c)))
         (func-blk (make-block '^bb0 (list inst-add inst-ret)))
         (func-reg (make-region (list func-blk)))
         (func-op (make-func '$add (list (cons '%a (make-i32)) (cons '%b (make-i32))) (list (make-i32)) func-reg))
         (mod-inst (make-instruction 'func func-op #f '()))
         (mod-blk (make-block '^bb0 (list mod-inst)))
         (mod-reg (make-region (list mod-blk)))
         (mod (make-module '$math mod-reg))

         ;; Emit binary WASM
         (bv (emit-wasm-binary mod)))

    (test-assert (bytevector? bv))
    (test-assert (> (bytevector-length bv) 8))
    (test-equal #x00 (bytevector-u8-ref bv 0))
    (test-equal #x61 (bytevector-u8-ref bv 1))
    (test-equal #x73 (bytevector-u8-ref bv 2))
    (test-equal #x6d (bytevector-u8-ref bv 3))

    ;; Emit to file
    (emit-wasm-binary-file mod "/home/yotto/dev/Ockham/scratch/math.wasm")))

(test-end "ockham-wasm-binary-emit")
