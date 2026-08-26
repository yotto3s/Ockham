#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*-
#!r6rs

(import (rnrs (6))
        (srfi :64 testing)
        (ockham core)
        (ockham ops))

(define-record-type (test-copy make-test-copy test-copy?)
  (fields
    (immutable value test-copy-value)))
(define (test-copy-serialize op) `(test-copy ,(test-copy-value op)))
(define (test-copy-deserialize lst)
  (if (and (pair? lst) (eq? (car lst) 'test-copy))
      (make-test-copy (cadr lst))
      #f))

(test-begin "ockham-ops")

(register-op 'test-copy test-copy-serialize test-copy-deserialize)

(test-group "constant-serialization"
  (let* ((c1 (make-constant 100))
         (c2 (make-constant (make-i32) 42))
         (s1 (constant-serialize c1))
         (s2 (constant-serialize c2))
         (d1 (constant-deserialize s1))
         (d2 (constant-deserialize s2)))
    (test-equal '(constant 100) s1)
    (test-equal '(constant :i32 42) s2)
    (test-assert (constant? d1))
    (test-assert (constant? d2))
    (test-equal 100 (constant-value d1))
    (test-equal 42 (constant-value d2))
    (test-assert (i32? (constant-type d2)))))

(test-group "constant-deserialization-invalid"
  (test-assert (not (constant-deserialize '(constant)))))

(test-group "constant-core-integration"
  (let* ((op-sexp '(%res :i32 = (constant :i32 42)))
         (op (read-instruction op-sexp)))
    (test-assert (instruction? op))
    (test-equal 'constant (instruction-op-type op))
    (test-assert (constant? (instruction-op op)))
    (test-equal 42 (constant-value (instruction-op op)))
    (test-equal op-sexp (instruction-serialize op))))

(test-group "copy-serialization"
  (let* ((c (make-copy '%a))
         (s (copy-serialize c))
         (d (copy-deserialize s)))
    (test-equal '(copy %a) s)
    (test-assert (copy? d))
    (test-equal '%a (copy-operand d))))

(test-group "copy-core-integration"
  (let* ((op-sexp '(%res :i32 = (copy %src)))
         (op (read-instruction op-sexp)))
    (test-assert (instruction? op))
    (test-equal 'copy (instruction-op-type op))
    (test-assert (copy? (instruction-op op)))
    (test-equal '%src (copy-operand (instruction-op op)))
    (test-equal op-sexp (instruction-serialize op))))

(test-group "arithmetic-serialization"
  (let* ((a (make-add (make-i32) '%d '%e))
         (s (add-serialize a))
         (d (add-deserialize s)))
    (test-equal '(add :i32 %d %e) s)
    (test-assert (add? d))
    (test-assert (i32? (add-type d)))
    (test-equal '%d (add-lhs d))
    (test-equal '%e (add-rhs d)))

  (let* ((sb (make-sub (make-i32) '%a '%b))
         (s (sub-serialize sb))
         (d (sub-deserialize s)))
    (test-equal '(sub :i32 %a %b) s)
    (test-assert (sub? d))
    (test-equal '%a (sub-lhs d))
    (test-equal '%b (sub-rhs d)))

  (let* ((m (make-mul (make-i32) '%x '%y))
         (sd (make-sdiv (make-i32) '%x '%y))
         (ud (make-udiv (make-i32) '%x '%y))
         (ls (make-lshift (make-i32) '%x '%amt))
         (rs (make-rshift (make-i32) '%x '%amt))
         (sr (make-srem (make-i32) '%x '%y))
         (ur (make-urem (make-i32) '%x '%y)))
    (test-equal '(mul :i32 %x %y) (mul-serialize m))
    (test-equal '(sdiv :i32 %x %y) (sdiv-serialize sd))
    (test-equal '(udiv :i32 %x %y) (udiv-serialize ud))
    (test-equal '(lshift :i32 %x %amt) (lshift-serialize ls))
    (test-equal '(rshift :i32 %x %amt) (rshift-serialize rs))
    (test-equal '(srem :i32 %x %y) (srem-serialize sr))
    (test-equal '(urem :i32 %x %y) (urem-serialize ur))

    (test-assert (mul? (mul-deserialize (mul-serialize m))))
    (test-assert (sdiv? (sdiv-deserialize (sdiv-serialize sd))))
    (test-assert (udiv? (udiv-deserialize (udiv-serialize ud))))
    (test-assert (lshift? (lshift-deserialize (lshift-serialize ls))))
    (test-assert (rshift? (rshift-deserialize (rshift-serialize rs))))
    (test-assert (srem? (srem-deserialize (srem-serialize sr))))
    (test-assert (urem? (urem-deserialize (urem-serialize ur))))))

(test-group "extension-serialization"
  (let* ((sx (make-sext '%x))
         (zx (make-zext '%x))
         (s-sx (sext-serialize sx))
         (s-zx (zext-serialize zx))
         (d-sx (sext-deserialize s-sx))
         (d-zx (zext-deserialize s-zx)))
    (test-equal '(sext %x) s-sx)
    (test-equal '(zext %x) s-zx)
    (test-assert (sext? d-sx))
    (test-assert (zext? d-zx))
    (test-equal '%x (sext-operand d-sx))
    (test-equal '%x (zext-operand d-zx))))

(test-group "load-serialization"
  (let* ((l1 (make-load '%ptr 8))
         (l2 (make-load '%ptr 0))
         (s1 (load-serialize l1))
         (s2 (load-serialize l2))
         (d1 (load-deserialize s1))
         (d2 (load-deserialize s2))
         (d3 (load-deserialize '(load %ptr))))
    (test-equal '(load %ptr 8) s1)
    (test-equal '(load %ptr) s2)
    (test-assert (load? d1))
    (test-assert (load? d2))
    (test-assert (load? d3))
    (test-equal '%ptr (load-ptr d1))
    (test-equal 8 (load-offset d1))
    (test-equal 0 (load-offset d2))
    (test-equal 0 (load-offset d3))))

(test-group "store-serialization"
  (let* ((st1 (make-store '%ptr '%val 16))
         (st2 (make-store '%ptr '%val 0))
         (s1 (store-serialize st1))
         (s2 (store-serialize st2))
         (d1 (store-deserialize s1))
         (d2 (store-deserialize s2))
         (d3 (store-deserialize '(store %ptr %val))))
    (test-equal '(store %ptr %val 16) s1)
    (test-equal '(store %ptr %val) s2)
    (test-assert (store? d1))
    (test-assert (store? d2))
    (test-assert (store? d3))
    (test-equal '%ptr (store-ptr d1))
    (test-equal '%val (store-val d1))
    (test-equal 16 (store-offset d1))
    (test-equal 0 (store-offset d2))
    (test-equal 0 (store-offset d3))))

(test-group "load-store-core-integration"
  (let* ((op-load-sexp '(%res :i32 = (load %ptr 8)))
         (op-store-sexp '(store %ptr %val 4))
         (op-load (read-instruction op-load-sexp))
         (op-store (read-instruction op-store-sexp)))
    (test-assert (instruction? op-load))
    (test-equal 'load (instruction-op-type op-load))
    (test-assert (load? (instruction-op op-load)))
    (test-equal op-load-sexp (instruction-serialize op-load))

    (test-assert (instruction? op-store))
    (test-equal 'store (instruction-op-type op-store))
    (test-assert (store? (instruction-op op-store)))
    (test-equal op-store-sexp (instruction-serialize op-store))))

(test-group "br-serialization"
  (let* ((j (make-br '^bb1 '(%x %y)))
         (s (br-serialize j))
         (d (br-deserialize s)))
    (test-equal '(br (^bb1 %x %y)) s)
    (test-assert (br? d))
    (test-equal '^bb1 (br-target d))
    (test-equal '(%x %y) (br-args d))))

(test-group "br-cond-serialization"
  (let* ((b (make-br-cond '%cond '^bb1 '(%x) '^bb2 '(%y)))
         (s (br-cond-serialize b))
         (d (br-cond-deserialize s)))
    (test-equal '(br-cond %cond (^bb1 %x) (^bb2 %y)) s)
    (test-assert (br-cond? d))
    (test-equal '%cond (br-cond-condition d))
    (test-equal '^bb1 (br-cond-then-target d))
    (test-equal '(%x) (br-cond-then-args d))
    (test-equal '^bb2 (br-cond-else-target d))
    (test-equal '(%y) (br-cond-else-args d))))

(test-group "control-flow-core-integration"
  (let* ((op-br-sexp '(br (^bb1 %a %b)))
         (op-brcond-sexp '(br-cond %c (^bb1 %a) (^bb2)))
         (op-br (read-instruction op-br-sexp))
         (op-brcond (read-instruction op-brcond-sexp)))
    (test-assert (instruction? op-br))
    (test-equal 'br (instruction-op-type op-br))
    (test-assert (br? (instruction-op op-br)))
    (test-equal op-br-sexp (instruction-serialize op-br))

    (test-assert (instruction? op-brcond))
    (test-equal 'br-cond (instruction-op-type op-brcond))
    (test-assert (br-cond? (instruction-op op-brcond)))
    (test-equal op-brcond-sexp (instruction-serialize op-brcond))))

(test-group "register-operand-assertions"
  (reset-error-log!)
  ;; Valid register operands: no error logged
  (add-serialize (make-add '%a '%b))
  (test-equal 0 (error-count))

  ;; Invalid (non-register) operand in add: logs error via okm-assert
  (add-serialize (make-add '%a 123))
  (test-equal 1 (error-count))

  ;; Block label (^bb1) is excluded, but non-register block arg (123) logs error
  (br-serialize (make-br '^bb1 '(123)))
  (test-equal 2 (error-count))

  (reset-error-log!))

(test-group "deserializer-assertions"
  (reset-error-log!)
  ;; Deserializing invalid non-register operand in add logs error and returns #f
  (test-assert (not (add-deserialize '(add %a 123))))
  (test-equal 1 (error-count))

  ;; Deserializing non-register in syscall logs error and returns #f
  (test-assert (not (syscall-deserialize '(syscall 1 %fd 123))))
  (test-equal 2 (error-count))

  ;; Deserializing non-symbol function name logs error and returns #f
  (test-assert (not (func-deserialize '(func invalid_name ((%a :i32)) -> :i32 (region (block ^bb0))))))
  (test-equal 3 (error-count))

  (reset-error-log!))

(test-group "syscall-serialization"
  (let* ((sc1 (make-syscall 60 '(%status)))
         (s1 (syscall-serialize sc1))
         (d1 (syscall-deserialize s1))
         (sc2 (make-syscall 1 '(%fd %buf %count)))
         (s2 (syscall-serialize sc2))
         (d2 (syscall-deserialize s2))
         (sc0 (make-syscall 39 '()))
         (s0 (syscall-serialize sc0))
         (d0 (syscall-deserialize s0)))
    (test-equal '(syscall 60 %status) s1)
    (test-equal '(syscall 1 %fd %buf %count) s2)
    (test-equal '(syscall 39) s0)
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
  (test-assert (not (syscall-deserialize '(syscall "not-an-id" %a))))
  (test-assert (not (syscall-deserialize '(syscall 1 %r1 %r2 %r3 %r4 %r5 %r6 %r7)))))

(test-group "syscall-core-integration"
  (let* ((op-sys-sexp '(syscall 1 %fd %buf %count))
         (op (read-instruction op-sys-sexp)))
    (test-assert (instruction? op))
    (test-equal 'syscall (instruction-op-type op))
    (test-assert (syscall? (instruction-op op)))
    (test-equal 1 (syscall-id (instruction-op op)))
    (test-equal '(%fd %buf %count) (syscall-args (instruction-op op)))
    (test-equal op-sys-sexp (instruction-serialize op))))

(test-group "call-serialization"
  (let* ((c1 (make-call '$fib '(%a)))
         (s1 (call-serialize c1))
         (d1 (call-deserialize s1))
         (c2 (make-call '%fn_ptr '(%x %y)))
         (s2 (call-serialize c2))
         (d2 (call-deserialize s2)))
    (test-equal '(call $fib %a) s1)
    (test-equal '(call %fn_ptr %x %y) s2)
    (test-assert (call? d1))
    (test-equal '$fib (call-callee d1))
    (test-equal '(%a) (call-args d1))
    (test-assert (call? d2))
    (test-equal '%fn_ptr (call-callee d2))
    (test-equal '(%x %y) (call-args d2))))

(test-group "call-core-integration"
  (let* ((op-call-sexp '(%res :i32 = (call $fib %a)))
         (op (read-instruction op-call-sexp)))
    (test-assert (instruction? op))
    (test-equal 'call (instruction-op-type op))
    (test-assert (call? (instruction-op op)))
    (test-equal '$fib (call-callee (instruction-op op)))
    (test-equal op-call-sexp (instruction-serialize op))))

(test-group "ret-serialization"
  (let* ((r1 (make-ret '(%r1 %r2)))
         (s1 (ret-serialize r1))
         (d1 (ret-deserialize s1))
         (r0 (make-ret))
         (s0 (ret-serialize r0))
         (d0 (ret-deserialize s0)))
    (test-equal '(ret %r1 %r2) s1)
    (test-equal '(ret) s0)
    (test-assert (ret? d1))
    (test-equal '(%r1 %r2) (ret-args d1))
    (test-assert (ret? d0))
    (test-equal '() (ret-args d0))))

(test-group "ret-core-integration"
  (let* ((op-ret-sexp '(ret %r1))
         (op (read-instruction op-ret-sexp)))
    (test-assert (instruction? op))
    (test-equal 'ret (instruction-op-type op))
    (test-assert (ret? (instruction-op op)))
    (test-equal '(%r1) (ret-args (instruction-op op)))
    (test-equal op-ret-sexp (instruction-serialize op))))

(test-group "func-serialization"
  (let* ((i32-t (make-i32))
         (i64-t (make-i64))
         (blk (make-block '^bb0 '()))
         (reg (make-region (list blk)))
         (f1 (make-func '$fib (list (cons '%a i32-t)) (list i32-t) reg))
         (s1 (func-serialize f1))
         (d1 (func-deserialize s1))
         (f2 (make-func '$div_mod (list (cons '%a i64-t) (cons '%b i64-t)) (list i64-t i64-t) reg))
         (s2 (func-serialize f2))
         (d2 (func-deserialize s2))
         (d3 (func-deserialize '(func $fib ((%a :i32)) -> (:i32) (region (block ^bb0)))))
         (f4 (make-func '$alloc (list (cons '%sz i64-t)) (list (make-ptr)) reg))
         (s4 (func-serialize f4))
         (d4 (func-deserialize s4)))
    (test-equal '(func $fib ((%a :i32)) -> (:i32) (region (block ^bb0))) s1)
    (test-equal '(func $div_mod ((%a :i64) (%b :i64)) -> (:i64 :i64) (region (block ^bb0))) s2)
    (test-equal '(func $alloc ((%sz :i64)) -> (:ptr) (region (block ^bb0))) s4)

    (test-assert (func? d1))
    (test-equal '$fib (func-name d1))
    (test-equal '%a (car (car (func-args d1))))
    (test-assert (i32? (cdr (car (func-args d1)))))
    (test-assert (i32? (car (func-return-types d1))))

    (test-assert (func? d2))
    (test-equal 2 (length (func-args d2)))
    (test-equal 2 (length (func-return-types d2)))

    (test-assert (func? d3))
    (test-equal 1 (length (func-return-types d3)))
    (test-assert (i32? (car (func-return-types d3))))

    (test-assert (func? d4))
    (test-assert (ptr? (car (func-return-types d4))))))

(test-group "func-core-integration"
  (let* ((op-func-sexp '(func $fib ((%a :i32)) -> (:i32) (region (block ^bb0))))
         (op (read-instruction op-func-sexp)))
    (test-assert (instruction? op))
    (test-equal 'func (instruction-op-type op))
    (test-assert (func? (instruction-op op)))
    (test-equal '$fib (func-name (instruction-op op)))
    (test-equal op-func-sexp (instruction-serialize op))))

(test-group "extern-serialization"
  (let* ((e1 (make-extern '$printf (make-i32)))
         (s1 (extern-serialize e1))
         (d1 (extern-deserialize s1))
         (fn-ty (make-func-type (list (make-ptr)) (list (make-i32))))
         (e2 (make-extern '$puts fn-ty))
         (s2 (extern-serialize e2))
         (d2 (extern-deserialize s2)))
    (test-equal '(extern $printf :i32) s1)
    (test-assert (extern? d1))
    (test-equal '$printf (extern-name d1))
    (test-assert (i32? (extern-type d1)))

    (test-equal '(extern $puts (func (:ptr) -> (:i32))) s2)
    (test-assert (extern? d2))
    (test-equal '$puts (extern-name d2))
    (test-assert (func-type? (extern-type d2)))))

(test-group "extern-core-integration"
  (let* ((op-sexp '(extern $malloc (func (:i64) -> (:ptr))))
         (op (read-instruction op-sexp)))
    (test-assert (instruction? op))
    (test-equal 'extern (instruction-op-type op))
    (test-assert (extern? (instruction-op op)))
    (test-equal '$malloc (extern-name (instruction-op op)))
    (test-assert (func-type? (extern-type (instruction-op op))))
    (test-equal op-sexp (instruction-serialize op))))

(test-group "module-serialization"
  (let* ((blk (make-block '^bb0 '()))
         (reg (make-region (list blk)))
         (m (make-module '$my_module reg))
         (s (module-serialize m))
         (d (module-deserialize s)))
    (test-equal '(module $my_module (region (block ^bb0))) s)
    (test-assert (module? d))
    (test-equal '$my_module (module-name d))
    (test-assert (region? (module-body d)))))

(test-group "module-core-integration"
  (let* ((op-sexp '(module $main_module (region (block ^bb0))))
         (op (read-instruction op-sexp)))
    (test-assert (instruction? op))
    (test-equal 'module (instruction-op-type op))
    (test-assert (module? (instruction-op op)))
    (test-equal '$main_module (module-name (instruction-op op)))
    (test-assert (region? (module-body (instruction-op op))))
    (test-equal op-sexp (instruction-serialize op))))

(test-end "ockham-ops")
