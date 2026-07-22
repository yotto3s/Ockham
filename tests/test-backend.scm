#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*-
#!r6rs

(import (rnrs (6))
        (srfi :64 testing)
        (ockham core)
        (ockham backend))

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

(test-group "be:copy-serialization"
  (let* ((c (make-copy (make-int 32) '%a))
         (s (copy-serialize c))
         (d (copy-deserialize s)))
    (test-equal '(be:copy %a : (int 32)) s)
    (test-assert (copy? d))
    (test-equal '%a (copy-operand d))
    (test-equal 32 (int-size (copy-type d)))))

(test-group "be:copy-core-integration"
  (let* ((op-sexp '(%res = (be:copy %src : (int 32))))
         (op (read-operation op-sexp #f)))
    (test-assert (operation? op))
    (test-equal 'be:copy (operation-op-type op))
    (test-assert (copy? (operation-op op)))
    (test-equal '%src (copy-operand (operation-op op)))
    (test-equal 32 (int-size (copy-type (operation-op op))))
    (test-equal op-sexp (operation-serialize op))))

(test-group "be:arithmetic-serialization"
  (let* ((i32 (make-int 32))
         (a (make-add i32 '%d '%e))
         (s (add-serialize a))
         (d (add-deserialize s)))
    (test-equal '(be:add %d %e : (int 32)) s)
    (test-assert (add? d))
    (test-equal '%d (add-lhs d))
    (test-equal '%e (add-rhs d))
    (test-equal 32 (int-size (add-type d))))

  (let* ((i64 (make-int 64))
         (sb (make-sub i64 '%a 1))
         (s (sub-serialize sb))
         (d (sub-deserialize s)))
    (test-equal '(be:sub %a 1 : (int 64)) s)
    (test-assert (sub? d))
    (test-equal '%a (sub-lhs d))
    (test-equal 1 (sub-rhs d))
    (test-equal 64 (int-size (sub-type d))))

  (let* ((i32 (make-int 32))
         (m (make-mul i32 '%x '%y))
         (id (make-idiv i32 '%x '%y))
         (ud (make-udiv i32 '%x '%y))
         (ls (make-lshift i32 '%x 2))
         (rs (make-rshift i32 '%x 2))
         (ir (make-irem i32 '%x '%y))
         (ur (make-urem i32 '%x '%y)))
    (test-equal '(be:mul %x %y : (int 32)) (mul-serialize m))
    (test-equal '(be:idiv %x %y : (int 32)) (idiv-serialize id))
    (test-equal '(be:udiv %x %y : (int 32)) (udiv-serialize ud))
    (test-equal '(be:lshift %x 2 : (int 32)) (lshift-serialize ls))
    (test-equal '(be:rshift %x 2 : (int 32)) (rshift-serialize rs))
    (test-equal '(be:irem %x %y : (int 32)) (irem-serialize ir))
    (test-equal '(be:urem %x %y : (int 32)) (urem-serialize ur))

    (test-assert (mul? (mul-deserialize (mul-serialize m))))
    (test-assert (idiv? (idiv-deserialize (idiv-serialize id))))
    (test-assert (udiv? (udiv-deserialize (udiv-serialize ud))))
    (test-assert (lshift? (lshift-deserialize (lshift-serialize ls))))
    (test-assert (rshift? (rshift-deserialize (rshift-serialize rs))))
    (test-assert (irem? (irem-deserialize (irem-serialize ir))))
    (test-assert (urem? (urem-deserialize (urem-serialize ur))))))

(test-group "be:extension-serialization"
  (let* ((i64 (make-int 64))
         (sx (make-sext i64 '%x))
         (zx (make-zext i64 '%x))
         (s-sx (sext-serialize sx))
         (s-zx (zext-serialize zx))
         (d-sx (sext-deserialize s-sx))
         (d-zx (zext-deserialize s-zx)))
    (test-equal '(be:sext %x : (int 64)) s-sx)
    (test-equal '(be:zext %x : (int 64)) s-zx)
    (test-assert (sext? d-sx))
    (test-assert (zext? d-zx))
    (test-equal '%x (sext-operand d-sx))
    (test-equal '%x (zext-operand d-zx))
    (test-equal 64 (int-size (sext-type d-sx)))
    (test-equal 64 (int-size (zext-type d-zx)))))

(test-group "define-dialect-op-custom-names"
  (let* ((c (make-be-test-copy 123))
         (s (serialize-op 'be:test-copy c))
         (d (deserialize-op s)))
    (test-assert (be-test-copy? c))
    (test-equal '(be:test-copy 123) s)
    (test-assert (be-test-copy? d))
    (test-equal 123 (test-copy-value d))))

(test-end "ockham-backend")

