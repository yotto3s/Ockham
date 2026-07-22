#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*-
#!r6rs

(import (rnrs (6))
        (srfi :64 testing)
        (ockham core)
        (ockham backend))

(test-begin "ockham-backend")

(test-group "be:constant-serialization"
  (let* ((c (make-constant (make-int 32) 100))
         (s (constant-serialize c))
         (d (constant-deserialize s)))
    (test-equal '(be:constant 100 : (int 32)) s)
    (test-assert (constant? d))
    (test-equal 100 (constant-value d))
    (test-equal 32 (int-size (constant-type d)))))

(test-group "be:constant-deserialization-invalid"
  (test-assert (not (constant-deserialize '(be:constant 42))))
  (test-assert (not (constant-deserialize '(be:constant 42 :))))
  (test-assert (not (constant-deserialize '(invalid-op 42 : (int 32))))))

(test-group "be:constant-core-integration"
  (let* ((op-sexp '(%res = (be:constant 42 : (int 32))))
         (op (read-operation op-sexp #f)))
    (test-assert (operation? op))
    (test-equal 'be:constant (operation-op-type op))
    (test-assert (constant? (operation-op op)))
    (test-equal 42 (constant-value (operation-op op)))
    (test-equal op-sexp (operation-serialize op))))

(define-dialect-op (be (test-copy make-be-test-copy be-test-copy?))
  (fields
    (immutable value test-copy-value))
  (serializer
    (lambda (op) `(be:test-copy ,(test-copy-value op))))
  (deserializer
    (lambda (lst)
      (if (and (pair? lst) (eq? (car lst) 'be:test-copy))
          (make-be-test-copy (cadr lst))
          #f))))

(test-group "define-dialect-op-custom-names"
  (let* ((c (make-be-test-copy 123))
         (s (serialize-op 'be:test-copy c))
         (d (deserialize-op s)))
    (test-assert (be-test-copy? c))
    (test-equal '(be:test-copy 123) s)
    (test-assert (be-test-copy? d))
    (test-equal 123 (test-copy-value d))))

(test-end "ockham-backend")
