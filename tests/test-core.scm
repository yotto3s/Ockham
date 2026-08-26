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

(test-group "primitive-types"
  (let ((t32 (make-i32))
        (t64 (make-i64))
        (f32 (make-f32))
        (f64 (make-f64))
        (p (make-ptr)))
    (test-assert (i32? t32))
    (test-assert (i64? t64))
    (test-assert (f32? f32))
    (test-assert (f64? f64))
    (test-assert (ptr? p))

    (test-equal ':i32 (i32-serialize t32))
    (test-equal ':i64 (i64-serialize t64))
    (test-equal ':f32 (f32-serialize f32))
    (test-equal ':f64 (f64-serialize f64))
    (test-equal ':ptr (ptr-serialize p))

    (test-assert (i32? (i32-deserialize ':i32)))
    (test-assert (i64? (i64-deserialize ':i64)))
    (test-assert (f32? (f32-deserialize ':f32)))
    (test-assert (f64? (f64-deserialize ':f64)))
    (test-assert (ptr? (ptr-deserialize ':ptr)))

    (test-assert (not (i32-deserialize 'invalid)))
    (test-assert (not (ptr-deserialize '(:ptr_invalid))))))

(test-group "func-type"
  (let* ((t32 (make-i32))
         (p (make-ptr))
         (ft (make-func-type (list t32 p) (list t32)))
         (s (func-type-serialize ft))
         (d (func-type-deserialize s))
         (d2 (deserialize-type '(func (:i32 :ptr) -> (:i32)))))
    (test-assert (func-type? ft))
    (test-equal '(func (:i32 :ptr) -> (:i32)) s)
    (test-assert (func-type? d))
    (test-equal 2 (length (func-type-param-types d)))
    (test-equal 1 (length (func-type-return-types d)))
    (test-assert (func-type? d2))
    (test-equal '(func (:i32 :ptr) -> (:i32)) (serialize-type d2))
    (test-assert (core-type? d2))))

(test-group "register"
  (test-assert (valid-register-name? '%rax))
  (test-assert (valid-register-name? '%1))
  (test-assert (not (valid-register-name? 'rax)))
  (test-assert (not (valid-register-name? '%)))
  (test-assert (not (valid-register-name? 123)))
  (let ((reg (make-register '%rax (make-i32))))
    (test-assert (register? reg))
    (test-equal '%rax (register-name reg))
    (test-assert (i32? (register-type reg)))))

(test-group "okm-symbol"
  (test-assert (okm-valid-symbol-name? '$foo))
  (test-assert (okm-valid-symbol-name? '$1))
  (test-assert (not (okm-valid-symbol-name? 'foo)))
  (test-assert (not (okm-valid-symbol-name? '$)))
  (test-assert (not (okm-valid-symbol-name? 123)))
  (let ((sym (make-okm-symbol '$foo (make-i32) #f)))
    (test-assert (okm-symbol? sym))
    (test-equal '$foo (okm-symbol-name sym))
    (test-assert (i32? (okm-symbol-type sym)))
    (test-equal #f (okm-symbol-def sym))))

(test-group "instruction-parsing"
  (let* ((op-lst '(%res :i32 = (test-op 42) attr1 attr2))
         (op (read-instruction op-lst)))
    (test-assert (instruction? op))
    (test-equal 'test-op (instruction-op-type op))
    (test-assert (test-op? (instruction-op op)))
    (test-equal 42 (test-op-value (instruction-op op)))
    (let ((reg (instruction-target op)))
      (test-assert (register? reg))
      (test-equal '%res (register-name reg))
      (test-assert (i32? (register-type reg))))
    (test-equal '(attr1 attr2) (instruction-attributes op)))

  (let* ((op-lst '(test-op 100))
         (op (read-instruction op-lst)))
    (test-assert (instruction? op))
    (test-equal 'test-op (instruction-op-type op))
    (test-assert (test-op? (instruction-op op)))
    (test-equal 100 (test-op-value (instruction-op op)))
    (test-assert (not (instruction-target op)))
    (test-equal '() (instruction-attributes op)))

  (let* ((op-lst '((test-op 100)))
         (op (read-instruction op-lst)))
    (test-assert (instruction? op))
    (test-equal 'test-op (instruction-op-type op))
    (test-assert (test-op? (instruction-op op)))
    (test-equal 100 (test-op-value (instruction-op op)))
    (test-assert (not (instruction-target op)))
    (test-equal '() (instruction-attributes op))))

(test-group "block-parsing"
  (let* ((block-lst '(block bb0
                      (%res :i32 = (test-op 42) attr1)
                      (test-op 100)))
         (blk (block-deserialize block-lst)))
    (test-assert (block? blk))
    (let ((name (block-name blk))
          (ops (block-instructions blk)))
      (test-equal 'bb0 name)
      (test-equal 2 (length ops))
      (let ((op1 (car ops))
            (op2 (cadr ops)))
        (test-assert (instruction? op1))
        (test-equal 'test-op (instruction-op-type op1))
        (test-assert (instruction? op2))
        (test-equal 'test-op (instruction-op-type op2)))))
  (test-assert (not (block-deserialize '(not-a-block)))))

(test-group "region-parsing"
  (let* ((region-lst '(region
                       (block
                         (%res :i32 = (test-op 42)))
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
                      (%res :i32 = (test-op 42) attr1)
                      (test-op 100)))
         (blk (block-deserialize block-lst)))
    (test-equal block-lst (block-serialize blk))))

(test-group "region-serialization"
  (let* ((region-lst '(region
                       (block bb0
                         (%res :i32 = (test-op 42)))
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
  (test-equal 'match (okm-match ':i32 (':i32 'match)))

  ;; okm-match: no matching pattern returns #f
  (test-equal #f (okm-match 'bad (':i32 'match)))

  (reset-error-log!)
  (test-equal 0 (error-count))
  (test-equal '() (error-messages)))

(test-group "core-deserializer-assertions"
  (reset-error-log!)
  ;; Deserializing invalid symbol returns #f
  (test-assert (not (deserialize-type 'not-a-type)))
  (test-equal 0 (error-count))

  ;; Deserializing invalid region tag returns #f
  (test-assert (not (region-deserialize '(not-a-region))))
  (test-equal 0 (error-count))

  ;; Deserializing invalid block tag returns #f
  (test-assert (not (block-deserialize '(not-a-block))))
  (test-equal 0 (error-count))

  (reset-error-log!))

(test-group "core-type-registry"
  (test-assert (core-type? (make-i32)))
  (test-assert (core-type? (make-i64)))
  (test-assert (core-type? (make-f32)))
  (test-assert (core-type? (make-f64)))
  (test-assert (core-type? (make-ptr)))
  (test-assert (not (core-type? "not-a-type")))
  (test-assert (not (core-type? 123)))

  (test-equal ':i32 (serialize-type (make-i32)))
  (test-equal ':i64 (serialize-type (make-i64)))
  (test-equal ':f32 (serialize-type (make-f32)))
  (test-equal ':f64 (serialize-type (make-f64)))
  (test-equal ':ptr (serialize-type (make-ptr)))

  (test-assert (i32? (deserialize-type ':i32)))
  (test-assert (i64? (deserialize-type ':i64)))
  (test-assert (f32? (deserialize-type ':f32)))
  (test-assert (f64? (deserialize-type ':f64)))
  (test-assert (ptr? (deserialize-type ':ptr)))
  (test-assert (not (deserialize-type '(invalid-type)))))

(test-end "ockham-core")
