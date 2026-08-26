(library (ockham wasm-binary-emit)
  (export
    emit-wasm-binary
    emit-wasm-binary-file
    wasm-binary-emit-pass)
  (import (rnrs (6))
          (ockham core)
          (ockham ops)
          (ockham pass-manager))

  (define (encode-uleb128 n)
    (let loop ((val n) (acc '()))
      (let ((byte (bitwise-and val #x7f))
            (next (bitwise-arithmetic-shift-right val 7)))
        (if (zero? next)
            (reverse (cons byte acc))
            (loop next (cons (bitwise-ior byte #x80) acc))))))

  (define (encode-sleb128 n)
    (let loop ((val n) (acc '()))
      (let* ((byte (bitwise-and val #x7f))
             (next (bitwise-arithmetic-shift-right val 7))
             (sign-bit (not (zero? (bitwise-and byte #x40)))))
        (if (or (and (zero? next) (not sign-bit))
                (and (= next -1) sign-bit))
            (reverse (cons byte acc))
            (loop next (cons (bitwise-ior byte #x80) acc))))))

  (define (type->byte ty)
    (cond
      ((i32? ty) #x7f)
      ((i64? ty) #x7e)
      ((f32? ty) #x7d)
      ((f64? ty) #x7c)
      ((ptr? ty) #x7f)
      (else #x7f)))

  (define (make-section id payload)
    (if (null? payload)
        '()
        (append (list id) (encode-uleb128 (length payload)) payload)))

  (define (symbol-strip-prefix sym prefix-char)
    (if (not (symbol? sym))
        sym
        (let ((str (symbol->string sym)))
          (if (and (> (string-length str) 0) (char=? (string-ref str 0) prefix-char))
              (string->symbol (substring str 1 (string-length str)))
              sym))))

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
      ((rshift? op) (rshift-lhs op))
      ((cmpeq? op) (cmpeq-lhs op))
      ((cmpne? op) (cmpne-lhs op))
      ((slt? op) (slt-lhs op))
      ((ult? op) (ult-lhs op))
      ((sgt? op) (sgt-lhs op))
      ((ugt? op) (ugt-lhs op))
      ((sle? op) (sle-lhs op))
      ((ule? op) (ule-lhs op))
      ((sge? op) (sge-lhs op))
      ((uge? op) (uge-lhs op))
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
      ((cmpeq? op) (cmpeq-rhs op))
      ((cmpne? op) (cmpne-rhs op))
      ((slt? op) (slt-rhs op))
      ((ult? op) (ult-rhs op))
      ((sgt? op) (sgt-rhs op))
      ((ugt? op) (ugt-rhs op))
      ((sle? op) (sle-rhs op))
      ((ule? op) (ule-rhs op))
      ((sge? op) (sge-rhs op))
      ((uge? op) (uge-rhs op))
      (else #f)))

  (define (binary-op->opcode op-name)
    (cond
      ((eq? op-name 'add) #x6a)
      ((eq? op-name 'sub) #x6b)
      ((eq? op-name 'mul) #x6c)
      ((eq? op-name 'sdiv) #x6d)
      ((eq? op-name 'udiv) #x6e)
      ((eq? op-name 'srem) #x6f)
      ((eq? op-name 'urem) #x70)
      ((eq? op-name 'lshift) #x74)
      ((eq? op-name 'rshift) #x75)
      ((eq? op-name 'cmpeq) #x46)
      ((eq? op-name 'cmpne) #x47)
      ((eq? op-name 'slt) #x48)
      ((eq? op-name 'ult) #x49)
      ((eq? op-name 'sgt) #x4a)
      ((eq? op-name 'ugt) #x4b)
      ((eq? op-name 'sle) #x4c)
      ((eq? op-name 'ule) #x4d)
      ((eq? op-name 'sge) #x4e)
      ((eq? op-name 'uge) #x4f)
      (else #x6a)))

  (define (emit-func-body func-op func-index-map)
    (let* ((args (func-args func-op))
           (body (func-body func-op))
           (blocks (region-blocks body))
           (num-blocks (length blocks))
           (param-names (map car args))
           (locals-map '())
           (local-count 0)
           (block-index-map '()))

      ;; Map blocks to indices
      (let loop ((blks blocks) (idx 0))
        (unless (null? blks)
          (set! block-index-map (cons (cons (block-name (car blks)) idx) block-index-map))
          (loop (cdr blks) (+ idx 1))))

      ;; Index parameters
      (for-each
        (lambda (a)
          (set! locals-map (cons (cons (car a) local-count) locals-map))
          (set! local-count (+ local-count 1)))
        args)

      ;; Collect locals from instruction targets
      (for-each
        (lambda (blk)
          (for-each
            (lambda (inst)
              (let ((target (instruction-target inst)))
                (when target
                  (let ((reg (register-name target)))
                    (unless (assoc reg locals-map)
                      (set! locals-map (cons (cons reg local-count) locals-map))
                      (set! local-count (+ local-count 1)))))))
            (block-instructions blk)))
        blocks)

      (let ((get-local-idx (lambda (r)
                             (let ((entry (assoc r locals-map)))
                               (if entry (cdr entry) 0))))
            (get-block-idx (lambda (b)
                             (let ((entry (assoc b block-index-map)))
                               (if entry (cdr entry) 0))))
            (code-bytes '()))

        ;; Emit WASM block headers if num-blocks > 1
        (when (> num-blocks 1)
          (let loop ((i 0))
            (when (< i (- num-blocks 1))
              (set! code-bytes (append code-bytes '(#x02 #x40))) ; block block_type=void
              (loop (+ i 1)))))

        ;; Emit block instructions
        (let loop ((blks blocks) (curr-idx 0))
          (unless (null? blks)
            (let ((blk (car blks)))
              (for-each
                (lambda (inst)
                  (let ((op-type-tag (instruction-op-type inst))
                        (op (instruction-op inst))
                        (target (instruction-target inst)))
                    (cond
                      ((eq? op-type-tag 'constant)
                       (let ((val (constant-value op)))
                         (set! code-bytes (append code-bytes (cons #x41 (encode-sleb128 val))))
                         (when target
                           (set! code-bytes (append code-bytes (cons #x21 (encode-uleb128 (get-local-idx (register-name target)))))))))

                      ((eq? op-type-tag 'copy)
                       (let ((src (copy-operand op)))
                         (set! code-bytes (append code-bytes (cons #x20 (encode-uleb128 (get-local-idx src)))))
                         (when target
                           (set! code-bytes (append code-bytes (cons #x21 (encode-uleb128 (get-local-idx (register-name target)))))))))

                      ((memq op-type-tag '(add sub mul sdiv udiv srem urem lshift rshift cmpeq cmpne slt ult sgt ugt sle ule sge uge))
                       (let ((lhs (get-binary-op-lhs op))
                             (rhs (get-binary-op-rhs op))
                             (opcode (binary-op->opcode op-type-tag)))
                         (set! code-bytes (append code-bytes (cons #x20 (encode-uleb128 (get-local-idx lhs)))))
                         (set! code-bytes (append code-bytes (cons #x20 (encode-uleb128 (get-local-idx rhs)))))
                         (set! code-bytes (append code-bytes (list opcode)))
                         (when target
                           (set! code-bytes (append code-bytes (cons #x21 (encode-uleb128 (get-local-idx (register-name target)))))))))

                      ((eq? op-type-tag 'call)
                       (let* ((callee (call-callee op))
                              (call-args-list (call-args op))
                              (f-entry (assoc callee func-index-map)))
                         (for-each
                           (lambda (arg)
                             (set! code-bytes (append code-bytes (cons #x20 (encode-uleb128 (get-local-idx arg))))))
                           call-args-list)
                         (when f-entry
                           (set! code-bytes (append code-bytes (cons #x10 (encode-uleb128 (cdr f-entry))))))
                         (when target
                           (set! code-bytes (append code-bytes (cons #x21 (encode-uleb128 (get-local-idx (register-name target)))))))))

                      ((eq? op-type-tag 'ret)
                       (let ((ret-args-list (ret-args op)))
                         (unless (null? ret-args-list)
                           (set! code-bytes (append code-bytes (cons #x20 (encode-uleb128 (get-local-idx (car ret-args-list)))))))
                         (set! code-bytes (append code-bytes '(#x0f)))))

                      ((eq? op-type-tag 'br)
                       (let* ((tgt (br-target op))
                              (tgt-idx (get-block-idx tgt))
                              (rel-depth (- tgt-idx curr-idx 1)))
                         (set! code-bytes (append code-bytes (cons #x0c (encode-uleb128 (max 0 rel-depth)))))))

                      ((eq? op-type-tag 'br-cond)
                       (let* ((cond-reg (br-cond-condition op))
                              (then-tgt (br-cond-then-target op))
                              (else-tgt (br-cond-else-target op))
                              (then-idx (get-block-idx then-tgt))
                              (else-idx (get-block-idx else-tgt))
                              (then-depth (- then-idx curr-idx 1))
                              (else-depth (- else-idx curr-idx 1)))
                         (set! code-bytes (append code-bytes (cons #x20 (encode-uleb128 (get-local-idx cond-reg)))))
                         (set! code-bytes (append code-bytes (cons #x0d (encode-uleb128 (max 0 then-depth)))))
                         (unless (= else-idx (+ curr-idx 1))
                           (set! code-bytes (append code-bytes (cons #x0c (encode-uleb128 (max 0 else-depth))))))))

                      (else '()))))
                (block-instructions blk))

              ;; Emit end opcode for block if not the last block
              (when (and (> num-blocks 1) (< curr-idx (- num-blocks 1)))
                (set! code-bytes (append code-bytes '(#x0b))))

              (loop (cdr blks) (+ curr-idx 1)))))

        ;; Append end opcode 0x0b for function body
        (set! code-bytes (append code-bytes '(#x0b)))

        ;; Locals declaration section for body
        (let* ((extra-locals (filter (lambda (entry) (>= (cdr entry) (length args))) locals-map))
               (locals-decl (if (null? extra-locals)
                                '(0)
                                (append (encode-uleb128 (length extra-locals))
                                        (apply append (map (lambda (e) (list 1 #x7f)) extra-locals)))))
               (func-payload (append locals-decl code-bytes)))
          (append (encode-uleb128 (length func-payload)) func-payload)))))

  (define (emit-wasm-binary module-obj)
    (okm-assert (module? module-obj))
    (let* ((body (module-body module-obj))
           (funcs '())
           (func-index-map '())
           (func-count 0)
           (type-entries '())
           (type-count 0)
           (func-type-indices '())
           (exports-entries '()))

      ;; Collect functions
      (for-each
        (lambda (blk)
          (for-each
            (lambda (inst)
              (let ((op-tag (instruction-op-type inst))
                    (op (instruction-op inst)))
                (when (eq? op-tag 'func)
                  (set! funcs (cons op funcs))
                  (set! func-index-map (cons (cons (func-name op) func-count) func-index-map))
                  (set! func-count (+ func-count 1)))))
            (block-instructions blk)))
        (region-blocks body))

      (set! funcs (reverse funcs))

      ;; Build Types, Funcs & Exports
      (for-each
        (lambda (f)
          (let* ((params (map (lambda (a) (type->byte (cdr a))) (func-args f)))
                 (results (map (lambda (r) (type->byte r)) (func-return-types f)))
                 (type-payload (append '(#x60)
                                       (cons (length params) params)
                                       (cons (length results) results))))
            (set! type-entries (append type-entries type-payload))
            (set! func-type-indices (append func-type-indices (encode-uleb128 type-count)))

            ;; Export entry
            (let* ((name-str (symbol->string (symbol-strip-prefix (func-name f) #\$)))
                   (name-bytes (map char->integer (string->list name-str)))
                   (f-idx (cdr (assoc (func-name f) func-index-map)))
                   (export-payload (append (encode-uleb128 (length name-bytes))
                                           name-bytes
                                           (cons 0 (encode-uleb128 f-idx)))))
              (set! exports-entries (append exports-entries export-payload)))

            (set! type-count (+ type-count 1))))
        funcs)

      ;; Sections
      (let* ((header '(#x00 #x61 #x73 #x6d #x01 #x00 #x00 #x00))
             (type-sec (make-section 1 (append (encode-uleb128 (length funcs)) type-entries)))
             (func-sec (make-section 3 (append (encode-uleb128 (length funcs)) func-type-indices)))
             (export-sec (make-section 7 (append (encode-uleb128 (length funcs)) exports-entries)))
             (code-bodies (apply append (map (lambda (f) (emit-func-body f func-index-map)) funcs)))
             (code-sec (make-section 10 (append (encode-uleb128 (length funcs)) code-bodies)))
             (full-bytes (append header type-sec func-sec export-sec code-sec)))
        (u8-list->bytevector full-bytes))))

  (define (emit-wasm-binary-file module-obj filepath)
    (let ((bv (emit-wasm-binary module-obj)))
      (call-with-port (open-file-output-port filepath (file-options replace))
        (lambda (port)
          (put-bytevector port bv)))))

  (define-pass wasm-binary-emit-pass (mod)
    "WASM Binary Emitter Pass: Converts an OKM module to a binary bytevector."
    (emit-wasm-binary mod))
)
