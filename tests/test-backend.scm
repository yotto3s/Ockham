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
         (sb (make-sub i64 '%a '%b))
         (s (sub-serialize sb))
         (d (sub-deserialize s)))
    (test-equal '(be:sub %a %b : (int 64)) s)
    (test-assert (sub? d))
    (test-equal '%a (sub-lhs d))
    (test-equal '%b (sub-rhs d))
    (test-equal 64 (int-size (sub-type d))))

  (let* ((i32 (make-int 32))
         (m (make-mul i32 '%x '%y))
         (id (make-idiv i32 '%x '%y))
         (ud (make-udiv i32 '%x '%y))
         (ls (make-lshift i32 '%x '%amt))
         (rs (make-rshift i32 '%x '%amt))
         (ir (make-irem i32 '%x '%y))
         (ur (make-urem i32 '%x '%y)))
    (test-equal '(be:mul %x %y : (int 32)) (mul-serialize m))
    (test-equal '(be:idiv %x %y : (int 32)) (idiv-serialize id))
    (test-equal '(be:udiv %x %y : (int 32)) (udiv-serialize ud))
    (test-equal '(be:lshift %x %amt : (int 32)) (lshift-serialize ls))
    (test-equal '(be:rshift %x %amt : (int 32)) (rshift-serialize rs))
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

(test-group "be:load-serialization"
  (let* ((i32 (make-int 32))
         (l1 (make-load i32 '%ptr 8))
         (l2 (make-load i32 '%ptr 0))
         (s1 (load-serialize l1))
         (s2 (load-serialize l2))
         (d1 (load-deserialize s1))
         (d2 (load-deserialize s2))
         (d3 (load-deserialize '(be:load %ptr : (int 32)))))
    (test-equal '(be:load %ptr 8 : (int 32)) s1)
    (test-equal '(be:load %ptr : (int 32)) s2)
    (test-assert (load? d1))
    (test-assert (load? d2))
    (test-assert (load? d3))
    (test-equal '%ptr (load-ptr d1))
    (test-equal 8 (load-offset d1))
    (test-equal 0 (load-offset d2))
    (test-equal 0 (load-offset d3))
    (test-equal 32 (int-size (load-type d1)))))

(test-group "be:store-serialization"
  (let* ((i64 (make-int 64))
         (st1 (make-store i64 '%ptr '%val 16))
         (st2 (make-store i64 '%ptr '%val 0))
         (s1 (store-serialize st1))
         (s2 (store-serialize st2))
         (d1 (store-deserialize s1))
         (d2 (store-deserialize s2))
         (d3 (store-deserialize '(be:store %ptr %val : (int 64)))))
    (test-equal '(be:store %ptr %val 16 : (int 64)) s1)
    (test-equal '(be:store %ptr %val : (int 64)) s2)
    (test-assert (store? d1))
    (test-assert (store? d2))
    (test-assert (store? d3))
    (test-equal '%ptr (store-ptr d1))
    (test-equal '%val (store-val d1))
    (test-equal 16 (store-offset d1))
    (test-equal 0 (store-offset d2))
    (test-equal 0 (store-offset d3))
    (test-equal 64 (int-size (store-type d1)))))

(test-group "be:load-store-core-integration"
  (let* ((op-load-sexp '(%res = (be:load %ptr 8 : (int 32))))
         (op-store-sexp '((be:store %ptr %val 4 : (int 32))))
         (op-load (read-operation op-load-sexp #f))
         (op-store (read-operation op-store-sexp #f)))
    (test-assert (operation? op-load))
    (test-equal 'be:load (operation-op-type op-load))
    (test-assert (load? (operation-op op-load)))
    (test-equal op-load-sexp (operation-serialize op-load))

    (test-assert (operation? op-store))
    (test-equal 'be:store (operation-op-type op-store))
    (test-assert (store? (operation-op op-store)))
    (test-equal op-store-sexp (operation-serialize op-store))))

(test-group "be:jmp-serialization"
  (let* ((j (make-jmp '(^bb1 %x %y)))
         (s (jmp-serialize j))
         (d (jmp-deserialize s)))
    (test-equal '(be:jmp (^bb1 %x %y)) s)
    (test-assert (jmp? d))
    (test-equal '(^bb1 %x %y) (jmp-target d))))

(test-group "be:br-cond-serialization"
  (let* ((b (make-br-cond '%cond '(^bb1 %x) '(^bb2 %y)))
         (s (br-cond-serialize b))
         (d (br-cond-deserialize s)))
    (test-equal '(be:br-cond %cond (^bb1 %x) (^bb2 %y)) s)
    (test-assert (br-cond? d))
    (test-equal '%cond (br-cond-condition d))
    (test-equal '(^bb1 %x) (br-cond-then-target d))
    (test-equal '(^bb2 %y) (br-cond-else-target d))))

(test-group "be:control-flow-core-integration"
  (let* ((op-jmp-sexp '((be:jmp (^bb1 %a %b))))
         (op-br-sexp '((be:br-cond %c (^bb1 %a) (^bb2))))
         (op-jmp (read-operation op-jmp-sexp #f))
         (op-br (read-operation op-br-sexp #f)))
    (test-assert (operation? op-jmp))
    (test-equal 'be:jmp (operation-op-type op-jmp))
    (test-assert (jmp? (operation-op op-jmp)))
    (test-equal op-jmp-sexp (operation-serialize op-jmp))

    (test-assert (operation? op-br))
    (test-equal 'be:br-cond (operation-op-type op-br))
    (test-assert (br-cond? (operation-op op-br)))
    (test-equal op-br-sexp (operation-serialize op-br))))

(test-group "be:register-operand-assertions"
  (reset-error-log!)
  (let ((i32 (make-int 32))
        (p (make-ptr)))
    ;; Valid register operands: no error logged
    (add-serialize (make-add i32 '%a '%b))
    (test-equal 0 (error-count))

    ;; Valid pointer type in add and sub: no error logged
    (add-serialize (make-add p '%a '%b))
    (sub-serialize (make-sub p '%a '%b))
    (test-equal 0 (error-count))
    (test-assert (add? (add-deserialize '(be:add %a %b : ptr))))
    (test-assert (sub? (sub-deserialize '(be:sub %a %b : ptr))))

    ;; Invalid (non-register) operand in add: logs error via okm-assert
    (add-serialize (make-add i32 '%a 123))
    (test-equal 1 (error-count))

    ;; Non-int / non-ptr type (e.g. invalid type object) in add logs error
    (add-serialize (make-add "invalid-type" '%a '%b))
    (test-equal 2 (error-count))

    ;; Invalid (non-int) type in mul: logs error via okm-assert
    (mul-serialize (make-mul p '%a '%b))
    (test-equal 3 (error-count))

    ;; Block label (^bb1) is excluded, but non-register block arg (123) logs error
    (jmp-serialize (make-jmp '(^bb1 123)))
    (test-equal 4 (error-count))

    (reset-error-log!)))

(test-group "be:deserializer-assertions"
  (reset-error-log!)
  ;; Deserializing invalid non-register operand in add logs error and returns #f
  (test-assert (not (add-deserialize '(be:add %a 123 : (int 32)))))
  (test-equal 1 (error-count))

  ;; Deserializing invalid type in mul logs error and returns #f
  (test-assert (not (mul-deserialize '(be:mul %a %b : ptr))))
  (test-equal 2 (error-count))

  ;; Deserializing non-register in syscall logs error and returns #f
  (test-assert (not (syscall-deserialize '(be:syscall 1 %fd 123))))
  (test-equal 3 (error-count))

  ;; Deserializing non-symbol function name logs error and returns #f
  (test-assert (not (func-deserialize '(be:func invalid_name ((%a : (int 32))) -> (int 32) (region (block ^bb0))))))
  (test-equal 4 (error-count))

  (reset-error-log!))

(test-group "be:syscall-serialization"
  (let* ((sc1 (make-syscall 60 '(%status)))
         (s1 (syscall-serialize sc1))
         (d1 (syscall-deserialize s1))
         (sc2 (make-syscall 1 '(%fd %buf %count)))
         (s2 (syscall-serialize sc2))
         (d2 (syscall-deserialize s2))
         (sc0 (make-syscall 39 '()))
         (s0 (syscall-serialize sc0))
         (d0 (syscall-deserialize s0)))
    (test-equal '(be:syscall 60 %status) s1)
    (test-equal '(be:syscall 1 %fd %buf %count) s2)
    (test-equal '(be:syscall 39) s0)
    (test-assert (syscall? d1))
    (test-assert (syscall? d2))
    (test-assert (syscall? d0))
    (test-equal 60 (syscall-id d1))
    (test-equal '(%status) (syscall-args d1))
    (test-equal 1 (syscall-id d2))
    (test-equal '(%fd %buf %count) (syscall-args d2))
    (test-equal 39 (syscall-id d0))
    (test-equal '() (syscall-args d0)))

  ;; Invalid deserialization (> 6 args or non-integer id)
  (test-assert (not (syscall-deserialize '(be:syscall "not-an-id" %a))))
  (test-assert (not (syscall-deserialize '(be:syscall 1 %r1 %r2 %r3 %r4 %r5 %r6 %r7)))))

(test-group "be:syscall-core-integration"
  (let* ((op-sys-sexp '((be:syscall 1 %fd %buf %count)))
         (op (read-operation op-sys-sexp #f)))
    (test-assert (operation? op))
    (test-equal 'be:syscall (operation-op-type op))
    (test-assert (syscall? (operation-op op)))
    (test-equal 1 (syscall-id (operation-op op)))
    (test-equal '(%fd %buf %count) (syscall-args (operation-op op)))
    (test-equal op-sys-sexp (operation-serialize op))))

(test-group "be:func-serialization"
  (let* ((i32 (make-int 32))
         (i64 (make-int 64))
         (blk (make-block '^bb0 '() #f))
         (reg (make-region (list blk) #f))
         (f1 (make-func '\x40;fib (list (cons '%a i32)) (list i32) reg))
         (s1 (func-serialize f1))
         (d1 (func-deserialize s1))
         (f2 (make-func '\x40;div_mod (list (cons '%a i64) (cons '%b i64)) (list i64 i64) reg))
         (s2 (func-serialize f2))
         (d2 (func-deserialize s2))
         (d3 (func-deserialize '(be:func \x40;fib ((%a : (int 32))) -> (int 32) (region (block ^bb0)))))
         (f4 (make-func '\x40;alloc (list (cons '%sz i64)) (list (make-ptr)) reg))
         (s4 (func-serialize f4))
         (d4 (func-deserialize s4)))
    (test-equal '(be:func \x40;fib ((%a : (int 32))) -> ((int 32)) (region (block ^bb0))) s1)
    (test-equal '(be:func \x40;div_mod ((%a : (int 64)) (%b : (int 64))) -> ((int 64) (int 64)) (region (block ^bb0))) s2)
    (test-equal '(be:func \x40;alloc ((%sz : (int 64))) -> (ptr) (region (block ^bb0))) s4)

    (test-assert (func? d1))
    (test-equal '\x40;fib (func-name d1))
    (test-equal '%a (car (car (func-args d1))))
    (test-equal 32 (int-size (cdr (car (func-args d1)))))
    (test-equal 32 (int-size (car (func-return-types d1))))

    (test-assert (func? d2))
    (test-equal 2 (length (func-args d2)))
    (test-equal 2 (length (func-return-types d2)))

    (test-assert (func? d3))
    (test-equal 1 (length (func-return-types d3)))
    (test-equal 32 (int-size (car (func-return-types d3))))

    (test-assert (func? d4))
    (test-assert (ptr? (car (func-return-types d4))))))

(test-group "be:func-core-integration"
  (let* ((op-func-sexp '((be:func \x40;fib ((%a : (int 32))) -> ((int 32)) (region (block ^bb0)))))
         (op (read-operation op-func-sexp #f)))
    (test-assert (operation? op))
    (test-equal 'be:func (operation-op-type op))
    (test-assert (func? (operation-op op)))
    (test-equal '\x40;fib (func-name (operation-op op)))
    (test-equal op-func-sexp (operation-serialize op))))

(test-group "define-dialect-op-custom-names"
  (let* ((c (make-be-test-copy 123))
         (s (serialize-op 'be:test-copy c))
         (d (deserialize-op s)))
    (test-assert (be-test-copy? c))
    (test-equal '(be:test-copy 123) s)
    (test-assert (be-test-copy? d))
    (test-equal 123 (test-copy-value d))))

(test-end "ockham-backend")


