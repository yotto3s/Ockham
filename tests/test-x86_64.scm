#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*-
#!r6rs

(import (rnrs (6))
        (srfi :64 testing)
        (ockham core)
        (ockham backend)
        (ockham x86_64))

(test-begin "ockham-x86_64")

(test-group "x86_64-abi"
  (test-assert (abi? x86_64-abi))
  (test-equal 'rsp (abi-sp-register x86_64-abi))
  (test-equal 'rbp (abi-fp-register x86_64-abi)))

(test-group "legalize-binary-ops"
  (let* ((mod-sexp
           '(be:module $mod_binary
              (region
                (block ^bb0
                  (%r2 : (int 32) = (be:add %r0 %r1))
                  (%r0 : (int 32) = (be:sub %r0 %r1))
                  (%r5 : (int 64) = (be:mul %r3 %r4))))))
         (expected
           '(be:module $mod_binary
              (region
                (block ^bb0
                  (%r2 : (int 32) = (be:copy %r0))
                  (%r2 : (int 32) = (be:add %r2 %r1))
                  (%r0 : (int 32) = (be:sub %r0 %r1))
                  (%r5 : (int 64) = (be:copy %r3))
                  (%r5 : (int 64) = (be:mul %r5 %r4))))))
         (mod (module-deserialize mod-sexp))
         (legal-mod (pass-legalize-two-address mod))
         (ser (module-serialize legal-mod)))
    (test-assert (module? legal-mod))
    (test-equal expected ser)))

(test-group "legalize-de-ssa-jmp"
  (let* ((mod-sexp
           '(be:module $mod_dessa_jmp
              (region
                (block ^bb0
                  ((be:func $foo ((%a : (int 32)) (%b : (int 32))) -> ((int 32))
                     (region
                       (block ^bb1
                         (%v1 : (int 32) = (be:add %a %b))
                         ((be:jmp (^bb_join %v1))))
                       (block ^bb2
                         (%v2 : (int 32) = (be:sub %a %b))
                         ((be:jmp (^bb_join %v2))))
                       (block (^bb_join (%v_param : (int 32)))
                         (%v_res : (int 32) = (be:add %v_param %c10))
                         ((be:ret %v_res))))))))))
         (expected
           '(be:module $mod_dessa_jmp
              (region
                (block ^bb0
                  ((be:func $foo ((%a : (int 32)) (%b : (int 32))) -> ((int 32))
                     (region
                       (block ^bb1
                         (%v1 : (int 32) = (be:copy %a))
                         (%v1 : (int 32) = (be:add %v1 %b))
                         (%v_param : (int 32) = (be:copy %v1))
                         ((be:jmp (^bb_join))))
                       (block ^bb2
                         (%v2 : (int 32) = (be:copy %a))
                         (%v2 : (int 32) = (be:sub %v2 %b))
                         (%v_param : (int 32) = (be:copy %v2))
                         ((be:jmp (^bb_join))))
                       (block ^bb_join
                         (%v_res : (int 32) = (be:copy %v_param))
                         (%v_res : (int 32) = (be:add %v_res %c10))
                         ((be:ret %v_res))))))))))
         (mod (module-deserialize mod-sexp))
         (legal-mod (pass-legalize-two-address mod))
         (ser (module-serialize legal-mod)))
    (test-assert (module? legal-mod))
    (test-equal expected ser)))

(test-group "legalize-de-ssa-br-cond"
  (let* ((mod-sexp
           '(be:module $mod_br_cond
              (region
                (block ^bb0
                  ((be:func $bar ((%c : (int 32)) (%a : (int 32)) (%b : (int 32))) -> ((int 32))
                     (region
                       (block ^bb_start
                         ((be:br-cond %c (^bb_then %a) (^bb_else %b))))
                       (block (^bb_then (%p1 : (int 32)))
                         ((be:ret %p1)))
                       (block (^bb_else (%p2 : (int 32)))
                         ((be:ret %p2))))))))))
         (expected
           '(be:module $mod_br_cond
              (region
                (block ^bb0
                  ((be:func $bar ((%c : (int 32)) (%a : (int 32)) (%b : (int 32))) -> ((int 32))
                     (region
                       (block ^bb_start
                         (%p1 : (int 32) = (be:copy %a))
                         (%p2 : (int 32) = (be:copy %b))
                         ((be:br-cond %c (^bb_then) (^bb_else))))
                       (block ^bb_then
                         ((be:ret %p1)))
                       (block ^bb_else
                         ((be:ret %p2))))))))))
         (mod (module-deserialize mod-sexp))
         (legal-mod (pass-legalize-two-address mod))
         (ser (module-serialize legal-mod)))
    (test-assert (module? legal-mod))
    (test-equal expected ser)))

(test-end "ockham-x86_64")
