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

(define (test-op-deserialize lst)
  (if (and (list? lst)
           (eq? (length lst) 2)
           (or (eq? (car lst) 'test-op) (eq? (car lst) 'temp-op)))
    (make-test-op (cadr lst))
    #f))

(test-op-serialize (make-test-op 1))
(test-op-deserialize '(test-op 1))

(test-group "op-registration"
  (test-assert (not (deserialize-op '(test-op-fail 1))))
  (test-assert (not (serialize-op 'test-op-fail '(1 2 3))))
  (let ((op (make-test-op 1)))
    (register-op 'test-op test-op-serialize test-op-deserialize)
    (test-equal '(test-op 1) (serialize-op 'test-op op))
    (test-assert
      (test-op=? (make-test-op 1)
                 (deserialize-op '(test-op 1))))
    ;; Test unregister-op with a temporary operator
    (register-op 'temp-op test-op-serialize test-op-deserialize)
    (test-equal '(test-op 1) (serialize-op 'temp-op op))
    (test-assert
      (test-op=? (make-test-op 1)
                 (deserialize-op '(temp-op 1))))
    (unregister-op 'temp-op)
    (test-assert (not (serialize-op 'temp-op op)))
    (test-assert (not (deserialize-op '(temp-op 1))))))

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

(test-group "func-type"
  (let* ((i32 (make-int 32))
         (p (make-ptr))
         (ft (make-func-type (list i32 p) (list i32)))
         (s (func-type-serialize ft))
         (d (func-type-deserialize s))
         (d2 (deserialize-type '(func ((int 32) ptr) -> ((int 32))))))
    (test-assert (func-type? ft))
    (test-equal '(func ((int 32) ptr) -> ((int 32))) s)
    (test-assert (func-type? d))
    (test-equal 2 (length (func-type-param-types d)))
    (test-equal 1 (length (func-type-return-types d)))
    (test-assert (func-type? d2))
    (test-equal '(func ((int 32) ptr) -> ((int 32))) (serialize-type d2))
    (test-assert (core-type? d2))))

(test-group "register"
  (test-assert (valid-register-name? '%rax))
  (test-assert (valid-register-name? '%1))
  (test-assert (not (valid-register-name? 'rax)))
  (test-assert (not (valid-register-name? '%)))
  (test-assert (not (valid-register-name? 123)))
  (let ((reg (make-register '%rax 'int)))
    (test-assert (register? reg))
    (test-equal '%rax (register-name reg))
    (test-equal 'int (register-type reg))))

(test-group "okm-symbol"
  (test-assert (okm-valid-symbol-name? '$foo))
  (test-assert (okm-valid-symbol-name? '$1))
  (test-assert (not (okm-valid-symbol-name? 'foo)))
  (test-assert (not (okm-valid-symbol-name? '$)))
  (test-assert (not (okm-valid-symbol-name? 123)))
  (let ((sym (make-okm-symbol '$foo 'int #f)))
    (test-assert (okm-symbol? sym))
    (test-equal '$foo (okm-symbol-name sym))
    (test-equal 'int (okm-symbol-type sym))
    (test-equal #f (okm-symbol-def sym))))

(test-group "operation-parsing"
  (let* ((op-lst '(%res : (int 32) = (test-op 42) attr1 attr2))
         (op (read-operation op-lst)))
    (test-assert (operation? op))
    (test-equal 'test-op (operation-op-type op))
    (test-assert (test-op? (operation-op op)))
    (test-equal 42 (test-op-value (operation-op op)))
    (let ((targets (operation-targets op)))
      (test-equal 1 (length targets))
      (let ((reg (car targets)))
        (test-assert (register? reg))
        (test-equal '%res (register-name reg))
        (test-assert (int? (register-type reg)))
        (test-equal 32 (int-size (register-type reg)))))
    (test-equal '(attr1 attr2) (operation-attributes op)))

  (let* ((op-lst '(%r1 %r2 : (int 32) ptr = (test-op 42)))
         (op (read-operation op-lst)))
    (test-assert (operation? op))
    (let ((targets (operation-targets op)))
      (test-equal 2 (length targets))
      (let ((reg1 (car targets))
            (reg2 (cadr targets)))
        (test-equal '%r1 (register-name reg1))
        (test-assert (int? (register-type reg1)))
        (test-equal '%r2 (register-name reg2))
        (test-assert (ptr? (register-type reg2))))))

  (let* ((op-lst '(test-op 100))
         (op (read-operation op-lst)))
    (test-assert (operation? op))
    (test-equal 'test-op (operation-op-type op))
    (test-assert (test-op? (operation-op op)))
    (test-equal 100 (test-op-value (operation-op op)))
    (test-equal '() (operation-targets op))
    (test-equal '() (operation-attributes op)))

  (let* ((op-lst '((test-op 100)))
         (op (read-operation op-lst)))
    (test-assert (operation? op))
    (test-equal 'test-op (operation-op-type op))
    (test-assert (test-op? (operation-op op)))
    (test-equal 100 (test-op-value (operation-op op)))
    (test-equal '() (operation-targets op))
    (test-equal '() (operation-attributes op))))

(test-group "block-parsing"
  (let* ((block-lst '(block bb0
                      (%res : (int 32) = (test-op 42) attr1)
                      (test-op 100)))
         (blk (block-deserialize block-lst)))
    (test-assert (block? blk))
    (let ((name (block-name blk))
          (ops (block-ops blk)))
      (test-equal 'bb0 name)
      (test-equal 2 (length ops))
      (let ((op1 (car ops))
            (op2 (cadr ops)))
        (test-assert (operation? op1))
        (test-equal 'test-op (operation-op-type op1))
        (test-assert (operation? op2))
        (test-equal 'test-op (operation-op-type op2)))))
  (test-assert (not (block-deserialize '(not-a-block)))))

(test-group "region-parsing"
  (let* ((region-lst '(region
                       (block
                         (%res : (int 32) = (test-op 42)))
                       (block
                         (test-op 100))))
         (reg (region-deserialize region-lst)))
    (test-assert (region? reg))
    (let ((blocks (region-blocks reg)))
      (test-equal 2 (length blocks))
      (let ((blk1 (car blocks))
            (blk2 (cadr blocks)))
        (test-assert (block? blk1))
        (test-assert (block? blk2)))))
  (test-assert (not (region-deserialize '(not-a-region)))))

(test-group "block-serialization"
  (let* ((block-lst '(block bb0
                      (%res : (int 32) = (test-op 42) attr1)
                      (test-op 100)))
         (blk (block-deserialize block-lst)))
    (test-equal block-lst (block-serialize blk))))

(test-group "region-serialization"
  (let* ((region-lst '(region
                       (block bb0
                         (%res : (int 32) = (test-op 42)))
                       (block bb1
                         (test-op 100))))
         (reg (region-deserialize region-lst)))
    (test-equal region-lst (region-serialize reg))))

(test-group "error-logging"
  (reset-error-log!)
  (test-equal 0 (error-count))
  (test-equal '() (error-messages))

  (log-error "Custom error message")
  (test-equal 1 (error-count))
  (test-equal '("Custom error message") (error-messages))

  (okm-assert (= 10 10))
  (test-equal 1 (error-count))

  (okm-assert (= 10 20))
  (test-equal 2 (error-count))
  (test-equal '("Custom error message" "Error: (= 10 20)") (error-messages))

  (reset-error-log!)
  (test-equal 'success (okm-assert-guard ((= 1 1) (= 2 2)) 'success))
  (test-equal 0 (error-count))

  (test-equal #f (okm-assert-guard ((= 1 1) (= 2 3)) 'success))
  (test-equal 1 (error-count))
  (test-equal '("Error: (= 2 3)") (error-messages))

  (reset-error-log!)
  (test-equal 0 (error-count))
  (test-equal '() (error-messages))

  ;; okm-match: matching pattern returns body
  (test-equal 32 (okm-match '(int 32) (('int sz) sz)))

  ;; okm-match: no matching pattern returns #f
  (test-equal #f (okm-match '(bad 32) (('int sz) sz)))

  ;; okm-match: multiple clauses, first match wins
  (test-equal 'int-case
    (okm-match '(int 32)
      (('int sz) 'int-case)
      (('ptr) 'ptr-case)))

  (test-equal 'ptr-case
    (okm-match '(ptr)
      (('int sz) 'int-case)
      (('ptr) 'ptr-case)))

  (reset-error-log!)
  (test-equal 0 (error-count))
  (test-equal '() (error-messages)))

(test-group "core-deserializer-assertions"
  (reset-error-log!)
  ;; Deserializing invalid size in int logs error and returns #f
  (test-assert (not (deserialize-type '(int -32))))
  (test-equal 1 (error-count))

  ;; Deserializing invalid symbol for ptr logs error and returns #f
  (test-assert (not (deserialize-type 'not-ptr)))
  (test-equal 1 (error-count))

  ;; Deserializing invalid region tag returns #f
  (test-assert (not (region-deserialize '(not-a-region))))
  (test-equal 1 (error-count))

  ;; Deserializing invalid block tag returns #f
  (test-assert (not (block-deserialize '(not-a-block))))
  (test-equal 1 (error-count))

  (reset-error-log!))

(test-group "core-type-registry"
  (test-assert (core-type? (make-int 32)))
  (test-assert (core-type? (make-ptr)))
  (test-assert (not (core-type? "not-a-type")))
  (test-assert (not (core-type? 123)))

  (test-equal '(int 32) (serialize-type (make-int 32)))
  (test-equal 'ptr (serialize-type (make-ptr)))

  (test-assert (int? (deserialize-type '(int 32))))
  (test-equal 32 (int-size (deserialize-type '(int 32))))
  (test-assert (ptr? (deserialize-type 'ptr)))
  (test-assert (not (deserialize-type '(invalid-type)))))

(test-end "ockham-core")
