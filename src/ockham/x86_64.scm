(library (ockham x86_64)
  (export x86_64-abi
          pass-legalize-two-address
          pass-lower-be-to-x86_64-asm)
  (import (rnrs (6))
          (ufo-match)
          (ockham core)
          (prefix (ockham backend) be:))

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

  (define (binary-op-lhs inner-op op-type)
    (case op-type
      ((be:add)    (be:add-lhs inner-op))
      ((be:sub)    (be:sub-lhs inner-op))
      ((be:mul)    (be:mul-lhs inner-op))
      ((be:idiv)   (be:idiv-lhs inner-op))
      ((be:udiv)   (be:udiv-lhs inner-op))
      ((be:lshift) (be:lshift-lhs inner-op))
      ((be:rshift) (be:rshift-lhs inner-op))
      ((be:irem)   (be:irem-lhs inner-op))
      ((be:urem)   (be:urem-lhs inner-op))
      (else #f)))

  (define (binary-op-update-lhs inner-op op-type new-lhs)
    (case op-type
      ((be:add)    (be:make-add new-lhs (be:add-rhs inner-op)))
      ((be:sub)    (be:make-sub new-lhs (be:sub-rhs inner-op)))
      ((be:mul)    (be:make-mul new-lhs (be:mul-rhs inner-op)))
      ((be:idiv)   (be:make-idiv new-lhs (be:idiv-rhs inner-op)))
      ((be:udiv)   (be:make-udiv new-lhs (be:udiv-rhs inner-op)))
      ((be:lshift) (be:make-lshift new-lhs (be:lshift-rhs inner-op)))
      ((be:rshift) (be:make-rshift new-lhs (be:rshift-rhs inner-op)))
      ((be:irem)   (be:make-irem new-lhs (be:irem-rhs inner-op)))
      ((be:urem)   (be:make-urem new-lhs (be:urem-rhs inner-op)))
      (else inner-op)))

  (define (extract-block-info blk)
    (let ((bname (block-name blk)))
      (if (symbol? bname)
          (values bname '())
          (if (and (pair? bname) (symbol? (car bname)))
              (let ((label (car bname))
                    (param-specs (cdr bname)))
                (let ((params
                        (map (lambda (spec)
                               (okm-match spec
                                 ((reg ': ty-sexp)
                                  (cons reg (deserialize-type ty-sexp)))
                                 (_ #f)))
                             param-specs)))
                  (values label params)))
              (values bname '())))))

  (define (create-copy-operation dst-name dst-type src-name)
    (let ((inner-copy (be:make-copy src-name))
          (reg (make-register dst-name dst-type)))
      (make-operation 'be:copy inner-copy (list reg) '())))

  (define (flatten-map proc lst)
    (apply append (map proc lst)))

  (define (legalize-operation op param-table)
    (cond
      ((eq? (operation-op-type op) 'be:func)
       (let* ((f (operation-op op))
              (f-name (be:func-name f))
              (f-args (be:func-args f))
              (f-rets (be:func-return-types f))
              (f-body (be:func-body f))
              (legalized-body (legalize-region f-body))
              (new-f (be:make-func f-name f-args f-rets legalized-body))
              (new-op (make-operation 'be:func new-f '() (operation-attributes op))))
         (list new-op)))

      ((memq (operation-op-type op) '(be:add be:sub be:mul be:idiv be:udiv be:lshift be:rshift be:irem be:urem))
       (let* ((op-type (operation-op-type op))
              (inner-op (operation-op op))
              (targets (operation-targets op))
              (dst-reg (car targets))
              (dst-name (register-name dst-reg))
              (dst-type (register-type dst-reg))
              (lhs (binary-op-lhs inner-op op-type)))
         (if (eq? dst-name lhs)
             (list op)
             (let* ((copy-op (create-copy-operation dst-name dst-type lhs))
                    (new-inner-op (binary-op-update-lhs inner-op op-type dst-name))
                    (mod-op (make-operation op-type new-inner-op targets (operation-attributes op))))
               (list copy-op mod-op)))))

      ((eq? (operation-op-type op) 'be:jmp)
       (let* ((inner-op (operation-op op))
              (tgt (be:jmp-target inner-op)))
         (if (or (null? (cdr tgt)) (not (pair? (cdr tgt))))
             (list op)
             (let* ((label (car tgt))
                    (args (cdr tgt))
                    (target-params (cdr (or (assq label param-table) (cons label '()))))
                    (copy-ops
                      (map (lambda (param-pair arg-name)
                             (let ((p-name (car param-pair))
                                   (p-type (cdr param-pair)))
                               (create-copy-operation p-name p-type arg-name)))
                            target-params args))
                    (new-inner-jmp (be:make-jmp (list label)))
                    (mod-jmp-op (make-operation 'be:jmp new-inner-jmp '() (operation-attributes op))))
               (append copy-ops (list mod-jmp-op))))))

      ((eq? (operation-op-type op) 'be:br-cond)
       (let* ((inner-op (operation-op op))
              (cond-reg (be:br-cond-condition inner-op))
              (then-t (be:br-cond-then-target inner-op))
              (else-t (be:br-cond-else-target inner-op))
              (then-label (car then-t))
              (then-args (cdr then-t))
              (else-label (car else-t))
              (else-args (cdr else-t))
              (then-params (cdr (or (assq then-label param-table) (cons then-label '()))))
              (else-params (cdr (or (assq else-label param-table) (cons else-label '()))))
              (then-copies
                (map (lambda (param-pair arg-name)
                       (create-copy-operation (car param-pair) (cdr param-pair) arg-name))
                     then-params then-args))
              (else-copies
                (map (lambda (param-pair arg-name)
                       (create-copy-operation (car param-pair) (cdr param-pair) arg-name))
                     else-params else-args))
              (new-inner-br (be:make-br-cond cond-reg (list then-label) (list else-label)))
              (mod-br-op (make-operation 'be:br-cond new-inner-br '() (operation-attributes op))))
         (append then-copies else-copies (list mod-br-op))))

      (else
       (list op))))

  (define (legalize-block blk param-table)
    (let-values (((label params) (extract-block-info blk)))
      (let* ((ops (block-ops blk))
             (new-ops
               (flatten-map
                 (lambda (op)
                   (legalize-operation op param-table))
                 ops)))
        (make-block label new-ops))))

  (define (legalize-region reg)
    (let* ((blocks (region-blocks reg))
           (param-table
             (map (lambda (blk)
                    (let-values (((label params) (extract-block-info blk)))
                      (cons label params)))
                  blocks))
           (new-blocks
             (map (lambda (blk)
                    (legalize-block blk param-table))
                  blocks)))
      (make-region new-blocks)))

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
      (let* ((mod-name (be:module-name module-ir))
             (mod-body (be:module-body module-ir))
             (new-body (legalize-region mod-body)))
        (be:make-module mod-name new-body))))

  ;; Not Impelemented yet!
  (define (pass-lower-be-to-x86_64-asm module-ir target-abi)
    (let* ((legal-ir   (pass-legalize-two-address module-ir)))
      ;; TODO: implement liveness, regalloc, frame layout, gas assembly emission
      legal-ir))
)
