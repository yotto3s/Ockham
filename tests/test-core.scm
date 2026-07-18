#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*-
#!r6rs

(import (rnrs (6))
        (srfi :64 testing)
        (ockham core))

(test-begin "ockham-core")

(define-record-type (test-op make-test-op test-op?)
  (fields
    (immutable value test-op-value)))

(define (test-op=? lhs rhs)
  (eq? (test-op-value lhs) (test-op-value rhs)))

(define (test-op-serialize op)
  `(test-op ,(test-op-value op)))

(define (test-op-deserialize args)
  (if (eq? (length args) 1)
    (make-test-op (car args))
    #f))

(test-op-serialize (make-test-op 1))
(test-op-deserialize '(1))

(test-group "op-registration"
  (test-assert (not (deserialize-op 'test-op-fail '(1))))
  (test-assert (not (serialize-op 'test-op-fail '(1 2 3))))
  (let ((op (make-test-op 1)))
    (register-op 'test-op test-op-serialize test-op-deserialize)
    (test-equal '(test-op 1) (serialize-op 'test-op op))
    (test-assert
      (test-op=? (make-test-op 1)
                 (deserialize-op 'test-op '(1))))
    ;; Test unregister-op with a temporary operator
    (register-op 'temp-op test-op-serialize test-op-deserialize)
    (test-equal '(test-op 1) (serialize-op 'temp-op op))
    (test-assert
      (test-op=? (make-test-op 1)
                 (deserialize-op 'temp-op '(1))))
    (unregister-op 'temp-op)
    (test-assert (not (serialize-op 'temp-op op)))
    (test-assert (not (deserialize-op 'temp-op '(1))))))

(test-group "int"
  (let ((i (make-int 32)))
    (test-assert (int? i))
    (test-equal 32 (int-size i))
    (test-equal '(int 32) (int-serialize i))
    (let ((deserialized (int-deserialize '(int 32))))
      (test-assert (int? deserialized))
      (test-equal 32 (int-size deserialized)))
    (test-assert (not (int-deserialize '(int))))
    (test-assert (not (int-deserialize '(ptr))))))

(test-group "ptr"
  (let ((p (make-ptr)))
    (test-assert (ptr? p))
    (test-equal 'ptr (ptr-serialize p))
    (let ((deserialized (ptr-deserialize 'ptr)))
      (test-assert (ptr? deserialized)))
    (test-assert (not (ptr-deserialize 'int)))
    (test-assert (not (ptr-deserialize '(ptr))))))

(test-group "register"
  (test-assert (valid-register-name? '%rax))
  (test-assert (valid-register-name? '%1))
  (test-assert (not (valid-register-name? 'rax)))
  (test-assert (not (valid-register-name? '%)))
  (test-assert (not (valid-register-name? 123)))
  (let ((reg (make-register '%rax 'int #f)))
    (test-assert (register? reg))
    (test-equal '%rax (register-name reg))
    (test-equal 'int (register-type reg))
    (test-equal #f (register-def reg))))

(test-group "okm-symbol"
  (test-assert (okm-valid-symbol-name? '\x40;foo))
  (test-assert (okm-valid-symbol-name? '\x40;1))
  (test-assert (not (okm-valid-symbol-name? 'foo)))
  (test-assert (not (okm-valid-symbol-name? '\x40;)))
  (test-assert (not (okm-valid-symbol-name? 123)))
  (let ((sym (make-okm-symbol '\x40;foo 'int #f)))
    (test-assert (okm-symbol? sym))
    (test-equal '\x40;foo (okm-symbol-name sym))
    (test-equal 'int (okm-symbol-type sym))
    (test-equal #f (okm-symbol-def sym))))

(test-group "abi-and-target"
  (let ((my-abi (make-abi '(%rax %rbx) '(%rax) '(%rbx) '%rsp '%rbp)))
    (test-assert (abi? my-abi))
    (test-equal '(%rax %rbx) (abi-general-registers my-abi))
    (test-equal '(%rax) (abi-caller-saved my-abi))
    (test-equal '(%rbx) (abi-callee-saved my-abi))
    (test-equal '%rsp (abi-sp-register my-abi))
    (test-equal '%rbp (abi-fp-register my-abi))
    (let ((tgt (make-target 'x86_64 'linux my-abi '())))
      (test-assert (target? tgt))
      (test-equal 'x86_64 (target-arch tgt))
      (test-equal 'linux (target-os tgt))
      (test-equal my-abi (target-abi tgt))
      (test-equal '() (target-constraints tgt)))))

(test-group "operation-parsing"
  (let* ((op-lst '(%res = (test-op 42) attr1 attr2))
         (op (read-operation op-lst #f)))
    (test-assert (operation? op))
    (test-equal 'test-op (operation-op-type op))
    (test-assert (test-op? (operation-op op)))
    (test-equal 42 (test-op-value (operation-op op)))
    (test-equal '(%res) (operation-targets op))
    (test-equal '(attr1 attr2) (operation-attributes op))
    (test-equal #f (operation-parent op)))

  (let* ((op-lst '((test-op 100)))
         (op (read-operation op-lst #f)))
    (test-assert (operation? op))
    (test-equal 'test-op (operation-op-type op))
    (test-assert (test-op? (operation-op op)))
    (test-equal 100 (test-op-value (operation-op op)))
    (test-equal '() (operation-targets op))
    (test-equal '() (operation-attributes op))
    (test-equal #f (operation-parent op))))

(test-group "block-parsing"
  (let* ((block-lst '(block
                      (%res = (test-op 42) attr1)
                      ((test-op 100))))
         (blk (read-block block-lst #f)))
    (test-assert (block? blk))
    (test-equal #f (block-parent blk))
    (let ((ops (block-ops blk)))
      (test-equal 2 (length ops))
      (let ((op1 (car ops))
            (op2 (cadr ops)))
        (test-assert (operation? op1))
        (test-equal 'test-op (operation-op-type op1))
        (test-equal blk (operation-parent op1))
        (test-assert (operation? op2))
        (test-equal 'test-op (operation-op-type op2))
        (test-equal blk (operation-parent op2)))))
  (test-assert (not (read-block '(not-a-block) #f))))

(test-group "region-parsing"
  (let* ((region-lst '(region
                       (block
                         (%res = (test-op 42)))
                       (block
                         ((test-op 100)))))
         (reg (read-region region-lst #f)))
    (test-assert (region? reg))
    (test-equal #f (region-parent reg))
    (set-region-parent! reg 'new-parent)
    (test-equal 'new-parent (region-parent reg))
    (set-region-parent! reg #f)
    (let ((blocks (region-blocks reg)))
      (test-equal 2 (length blocks))
      (set-region-blocks! reg '())
      (test-equal '() (region-blocks reg))
      (set-region-blocks! reg blocks)
      (let ((blk1 (car blocks))
            (blk2 (cadr blocks)))
        (test-assert (block? blk1))
        (test-equal reg (block-parent blk1))
        (set-block-parent! blk1 'new-blk-parent)
        (test-equal 'new-blk-parent (block-parent blk1))
        (set-block-parent! blk1 reg)
        (test-assert (block? blk2))
        (test-equal reg (block-parent blk2))
        (let ((ops (block-ops blk1)))
          (set-block-ops! blk1 '())
          (test-equal '() (block-ops blk1))
          (set-block-ops! blk1 ops)))))
  (test-assert (not (read-region '(not-a-region) #f))))

(test-end "ockham-core")

