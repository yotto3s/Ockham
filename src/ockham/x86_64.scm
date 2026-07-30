(library (ockham x86_64)
  (import (rnrs (6))
          (ufo-match)
          (ockham core)
          (prefix (ockham backend) be:)

  (define x86_64-abi
    (make-abi
      '(rax rdx rcx r8 rdi rsi r9 r10 r11 rbx r12 r13 r14 r15) ; general-registers
      '(rax rdx rcx r8 rdi rsi r9 r10 r11) ; caller-saved
      '(rbx r12 r13 r14 r15) ; callee-saved
      '(rdi rsi rdx rcx r8 r9) ; argument-registers
      '(rax rdx rcx r8) ; return-registers
      '((rax eax ax al) ; subregisters
        (rcx ecx cx cl)
        (rdx edx dx dl)
        (rbx ebx bx bl)
        (rsi esi si sil)
        (rdi edi di dil)
        (rsp esp sp spl)
        (rbp ebp bp bpl)
        (r8  r8d r8w r8b)
        (r9  r9d r9w r9b)
        (r10 r10d r10w r10b)
        (r11 r11d r11w r11b)
        (r12 r12d r12w r12b)
        (r13 r13d r13w r13b)
        (r14 r14d r14w r14b)
        (r15 r15d r15w r15b))
      'rsp ; stack-pointer
      'rbp ; frame-pointer
      ))

  ;; Transform SSA based IR to low level IR close to x86_64 instructions
  ;; 1) Transform 3-operands binary operators to 2-operands operators
  ;;     e.g.
  ;;          before (%r2 : (int 32) = (be:add %r0 %r1))
  ;;          after  (%r2 : (int 32) = (be:copy %r0))
  ;;                 (%r2 : (int 32) = (be:add %r2 %r1))
  ;; 2) Transform block arguments to register assignment
  ;; BEFORE De-SSA (SSA with Block Arguments)
  ;; ^bb1:
  ;;   (%v1 : (int 32) = (be:add %a %b))
  ;;   ((be:jmp ^bb_join (%v1))) ; Pass %v1 as argument
  ;; 
  ;; ^bb2:
  ;;   (%v2 : (int 32) = (be:sub %a %b))
  ;;   ((be:jmp ^bb_join (%v2))) ; Pass %v2 as argument
  ;; 
  ;; ^bb_join (%v_param : (int 32)):                  ; Receives %v1 or %v2
  ;;   (%v_res : (int 32) = (be:add %v_param 10))
  ;;
  ;; AFTER De-SSA (Non-SSA Machine IR)
  ;; ^bb1:
  ;;   (%v1 : (int 32) = (be:add %a %b))
  ;;   (%v_param : (int 32) = (be:copy %v1))             ; Explicit copy inserted!
  ;;   ((be:jmp ^bb_join))       ; Jump becomes parameterless
  ;; 
  ;; ^bb2:
  ;;   (%v2 : (int 32) = (be:sub %a %b))
  ;;   (%v_param : (int 32) = (be:copy %v2))             ; Explicit copy inserted!
  ;;   ((be:jmp ^bb_join))       ; Jump becomes parameterless
  ;; 
  ;; ^bb_join:                                ; Clean block with no arguments
  ;;   (%v_res : (int 32) = (be:add %v_param 10))
  ;;
  (define (pass-legalize-two-address module-ir)
    (okm-assert-guard
      ((be:module? module-ir))
      ;; TODO: Impelement the logic
      ))

  ;; Not Impelemented yet!
  (define (pass-lower-be-to-x86_64-asm module-ir target-abi)
    (let* ((legal-ir   (pass-legalize-two-address module-ir))
           (liveness   (pass-analyze-liveness legal-ir))
           (alloc-res  (pass-allocate-registers legal-ir liveness target-abi))
           (framed-ir  (pass-insert-frame-layout alloc-res target-abi)))
      ;; Emit the final assembly file
      (emit-gas-assembly framed-ir alloc-res)))
)
