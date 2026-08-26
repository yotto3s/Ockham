(library (ockham wasm-emit)
  (export
    emit-wasm-wat
    emit-wasm-wat-string
    wasm-emit-pass)
  (import (rnrs (6))
          (ufo-match)
          (ockham core)
          (ockham ops)
          (ockham pass-manager))

  (define (type->wat-type ty)
    (cond
      ((i32? ty) 'i32)
      ((i64? ty) 'i64)
      ((f32? ty) 'f32)
      ((f64? ty) 'f64)
      ((ptr? ty) 'i32)
      (else 'i32)))

  (define (reg->wat-sym reg-sym)
    (if (not (symbol? reg-sym))
        reg-sym
        (let ((str (symbol->string reg-sym)))
          (if (and (> (string-length str) 1) (char=? (string-ref str 0) #\%))
              (string->symbol (string-append "$" (substring str 1 (string-length str))))
              reg-sym))))

  (define (symbol-strip-prefix sym prefix-char)
    (if (not (symbol? sym))
        sym
        (let ((str (symbol->string sym)))
          (if (and (> (string-length str) 0) (char=? (string-ref str 0) prefix-char))
              (string->symbol (substring str 1 (string-length str)))
              sym))))

  (define (binary-op->wat-instr op-type op-name)
    (let ((prefix (symbol->string (type->wat-type op-type)))
          (suffix (cond
                    ((eq? op-name 'add) "add")
                    ((eq? op-name 'sub) "sub")
                    ((eq? op-name 'mul) "mul")
                    ((eq? op-name 'sdiv) "div_s")
                    ((eq? op-name 'udiv) "div_u")
                    ((eq? op-name 'srem) "rem_s")
                    ((eq? op-name 'urem) "rem_u")
                    ((eq? op-name 'lshift) "shl")
                    ((eq? op-name 'rshift) "shr_s")
                    (else (symbol->string op-name)))))
      (string->symbol (string-append prefix "." suffix))))

  (define (get-binary-op-lhs op)
    (cond
      ((add? op) (add-lhs op))
      ((sub? op) (sub-lhs op))
      ((mul? op) (mul-lhs op))
      ((sdiv? op) (sdiv-lhs op))
      ((udiv? op) (udiv-lhs op))
      ((srem? op) (srem-lhs op))
      ((urem? op) (urem-lhs op))
      ((lshift? op) (lshift-lhs op))
      ((rshift? op) (rshift-rhs op))
      (else #f)))

  (define (get-binary-op-rhs op)
    (cond
      ((add? op) (add-rhs op))
      ((sub? op) (sub-rhs op))
      ((mul? op) (mul-rhs op))
      ((sdiv? op) (sdiv-rhs op))
      ((udiv? op) (udiv-rhs op))
      ((srem? op) (srem-rhs op))
      ((urem? op) (urem-rhs op))
      ((lshift? op) (lshift-rhs op))
      ((rshift? op) (rshift-rhs op))
      (else #f)))

  (define (get-binary-op-type op)
    (cond
      ((add? op) (add-type op))
      ((sub? op) (sub-type op))
      ((mul? op) (mul-type op))
      ((sdiv? op) (sdiv-type op))
      ((udiv? op) (udiv-type op))
      ((srem? op) (srem-type op))
      ((urem? op) (urem-type op))
      ((lshift? op) (lshift-type op))
      ((rshift? op) (rshift-type op))
      (else #f)))

  (define (emit-instruction-wat inst)
    (let ((op-type-tag (instruction-op-type inst))
          (op (instruction-op inst))
          (target (instruction-target inst)))
      (cond
        ((eq? op-type-tag 'constant)
         (let* ((val (constant-value op))
                (ty (or (constant-type op) (and target (register-type target)) (make-i32)))
                (wat-ty (type->wat-type ty))
                (const-instr (string->symbol (string-append (symbol->string wat-ty) ".const")))
                (code (list (list const-instr val))))
           (if target
               (append code (list (list 'local.set (reg->wat-sym (register-name target)))))
               code)))

        ((eq? op-type-tag 'copy)
         (let* ((src (copy-operand op))
                (code (list (list 'local.get (reg->wat-sym src)))))
           (if target
               (append code (list (list 'local.set (reg->wat-sym (register-name target)))))
               code)))

        ((memq op-type-tag '(add sub mul sdiv udiv srem urem lshift rshift))
         (let* ((lhs (get-binary-op-lhs op))
                (rhs (get-binary-op-rhs op))
                (ty (or (get-binary-op-type op) (and target (register-type target)) (make-i32)))
                (wat-op (binary-op->wat-instr ty op-type-tag)))
           (let ((code (list (list 'local.get (reg->wat-sym lhs))
                             (list 'local.get (reg->wat-sym rhs))
                             (list wat-op))))
             (if target
                 (append code (list (list 'local.set (reg->wat-sym (register-name target)))))
                 code))))

        ((eq? op-type-tag 'load)
         (let* ((ptr (load-ptr op))
                (offset (load-offset op))
                (ty (if target (register-type target) (make-i32)))
                (wat-op (string->symbol (string-append (symbol->string (type->wat-type ty)) ".load")))
                (load-code (if (and offset (not (zero? offset)))
                               (list (list wat-op (string->symbol (string-append "offset=" (number->string offset)))))
                               (list (list wat-op))))
                (code (cons (list 'local.get (reg->wat-sym ptr)) load-code)))
           (if target
               (append code (list (list 'local.set (reg->wat-sym (register-name target)))))
               code)))

        ((eq? op-type-tag 'store)
         (let* ((ptr (store-ptr op))
                (val (store-val op))
                (offset (store-offset op))
                (wat-op 'i32.store)
                (store-code (if (and offset (not (zero? offset)))
                                (list (list wat-op (string->symbol (string-append "offset=" (number->string offset)))))
                                (list (list wat-op)))))
           (cons (list 'local.get (reg->wat-sym ptr))
                 (cons (list 'local.get (reg->wat-sym val)) store-code))))

        ((eq? op-type-tag 'call)
         (let* ((callee (call-callee op))
                (args (call-args op))
                (arg-codes (map (lambda (a) (list 'local.get (reg->wat-sym a))) args))
                (code (append arg-codes (list (list 'call callee)))))
           (if target
               (append code (list (list 'local.set (reg->wat-sym (register-name target)))))
               code)))

        ((eq? op-type-tag 'ret)
         (let ((args (ret-args op)))
           (if (null? args)
               '((return))
               (list (list 'local.get (reg->wat-sym (car args))) '(return)))))

        ((eq? op-type-tag 'br)
         (list (list 'br (br-target op))))

        ((eq? op-type-tag 'br-cond)
         (list (list 'local.get (reg->wat-sym (br-cond-condition op)))
               (list 'br_if (br-cond-then-target op))))

        (else '()))))

  (define (emit-func-wat func-op)
    (let* ((name (func-name func-op))
           (args (func-args func-op))
           (rets (func-return-types func-op))
           (body (func-body func-op))
           (params-wat (map (lambda (a)
                              (list 'param (reg->wat-sym (car a)) (type->wat-type (cdr a))))
                            args))
           (rets-wat (map (lambda (r) (list 'result (type->wat-type r))) rets))
           (param-names (map car args))
           (locals-table '())
           (instrs-wat '()))
      ;; Process body blocks & instructions
      (for-each
        (lambda (blk)
          (for-each
            (lambda (inst)
              (let ((target (instruction-target inst)))
                (when target
                  (let ((reg (register-name target))
                        (ty (register-type target)))
                    (unless (or (memq reg param-names) (assoc reg locals-table))
                      (set! locals-table (cons (cons reg ty) locals-table))))))
              (set! instrs-wat (append instrs-wat (emit-instruction-wat inst))))
            (block-instructions blk)))
        (region-blocks body))

      (let* ((export-name (symbol->string (symbol-strip-prefix name #\$)))
             (export-wat (list (list 'export export-name)))
             (locals-wat (map (lambda (l)
                                (list 'local (reg->wat-sym (car l)) (type->wat-type (cdr l))))
                              (reverse locals-table))))
        (cons 'func
              (cons name
                    (append export-wat
                            (append params-wat
                                    (append rets-wat
                                            (append locals-wat instrs-wat)))))))))

  (define (emit-extern-wat extern-op)
    (let ((name (extern-name extern-op))
          (ty (extern-type extern-op)))
      (if (func-type? ty)
          (let ((params-wat (map (lambda (p) (list 'param (type->wat-type p))) (func-type-param-types ty)))
                (rets-wat (map (lambda (r) (list 'result (type->wat-type r))) (func-type-return-types ty))))
            `(import "env" ,(symbol->string name) (func ,name ,@params-wat ,@rets-wat)))
          `(import "env" ,(symbol->string name) (global ,name (mut ,(type->wat-type ty)))))))

  (define (emit-wasm-wat module-obj)
    (okm-assert (module? module-obj))
    (let* ((name (module-name module-obj))
           (body (module-body module-obj))
           (elements '()))
      (for-each
        (lambda (blk)
          (for-each
            (lambda (inst)
              (let ((op-tag (instruction-op-type inst))
                    (op (instruction-op inst)))
                (cond
                  ((eq? op-tag 'func)
                   (set! elements (cons (emit-func-wat op) elements)))
                  ((eq? op-tag 'extern)
                   (set! elements (cons (emit-extern-wat op) elements))))))
            (block-instructions blk)))
        (region-blocks body))
      `(module ,name . ,(reverse elements))))

  (define (sexp->string sexp)
    (call-with-string-output-port
      (lambda (p)
        (write sexp p))))

  (define (emit-wasm-wat-string module-obj)
    (sexp->string (emit-wasm-wat module-obj)))

  (define-pass wasm-emit-pass (mod)
    "WASM WAT Emitter Pass: Converts an OKM module to WAT S-expressions."
    (emit-wasm-wat mod))
)
