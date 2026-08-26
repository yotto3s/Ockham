(library (ockham core)
  (export
    i32 make-i32 i32?
    i32-serialize i32-deserialize

    i64 make-i64 i64?
    i64-serialize i64-deserialize

    f32 make-f32 f32?
    f32-serialize f32-deserialize

    f64 make-f64 f64?
    f64-serialize f64-deserialize

    ptr make-ptr ptr?
    ptr-serialize ptr-deserialize

    func-type make-func-type func-type?
    func-type-param-types func-type-return-types
    func-type-serialize func-type-deserialize

    register make-register register?
    register-name register-type
    valid-register-name?

    okm-symbol make-okm-symbol okm-symbol?
    okm-symbol-name okm-symbol-type okm-symbol-def
    okm-valid-symbol-name?

    region make-region region?
    region-blocks
    region-serialize region-deserialize

    block make-block block?
    block-name block-instructions block-ops
    block-serialize block-deserialize

    instruction make-instruction instruction?
    instruction-op-type instruction-op instruction-target
    instruction-attributes
    read-instruction instruction-serialize

    register-op unregister-op serialize-op deserialize-op

    log-error okm-assert okm-assert-guard okm-match error-count error-messages reset-error-log!
    core-type? serialize-type deserialize-type)
  (import (rnrs (6))
          (ufo-match))

  ;; Error Logging System
  (define *error-count* 0)
  (define *error-messages* '())

  (define (log-error msg)
    (set! *error-count* (+ *error-count* 1))
    (set! *error-messages* (cons msg *error-messages*)))

  (define (error-count) *error-count*)
  (define (error-messages) (reverse *error-messages*))
  (define (reset-error-log!)
    (set! *error-count* 0)
    (set! *error-messages* '()))

  (define-syntax okm-assert
    (lambda (stx)
      (syntax-case stx ()
        ((_ expr)
         (let* ((datum (syntax->datum #'expr))
                (str (call-with-string-output-port
                       (lambda (p)
                         (display "Error: " p)
                         (write datum p)))))
           #`(unless expr
               (log-error #,str)))))))

  (define-syntax okm-assert-guard
    (syntax-rules ()
      ((_ (cond ...) body)
       (begin
         (okm-assert cond) ...
         (if (and cond ...)
             body
             #f)))))

  (define-syntax okm-match
    (syntax-rules ()
      ((_ expr (pattern body ...) ...)
       (match expr
         (pattern body ...) ...
         (_ #f)))))

  ;; Operation Registry
  (define *op-types* '())

  (define (register-op op serializer deserializer)
    (set! *op-types* (cons (list op serializer deserializer) *op-types*)))

  (define (unregister-op op)
    (set! *op-types* (remp (lambda (entry) (eq? (car entry) op)) *op-types*)))

  (define (serialize-op op-type op)
    (let ((entry (assoc op-type *op-types*)))
      (if entry
        ((cadr entry) op)
        #f)))

  (define (deserialize-op lst)
    (if (and (list? lst) (not (null? lst)))
      (let* ((op-type (car lst))
             (entry (assoc op-type *op-types*)))
        (if entry
          ((caddr entry) lst)
          #f))
      #f))

  ;; okm WASM Types: :i32, :i64, :f32, :f64, :ptr, func-type
  (define-record-type (i32 make-i32 i32?))
  (define (i32-serialize okm)
    (okm-assert (i32? okm))
    ':i32)
  (define (i32-deserialize sexp)
    (okm-match sexp (':i32 (make-i32))))

  (define-record-type (i64 make-i64 i64?))
  (define (i64-serialize okm)
    (okm-assert (i64? okm))
    ':i64)
  (define (i64-deserialize sexp)
    (okm-match sexp (':i64 (make-i64))))

  (define-record-type (f32 make-f32 f32?))
  (define (f32-serialize okm)
    (okm-assert (f32? okm))
    ':f32)
  (define (f32-deserialize sexp)
    (okm-match sexp (':f32 (make-f32))))

  (define-record-type (f64 make-f64 f64?))
  (define (f64-serialize okm)
    (okm-assert (f64? okm))
    ':f64)
  (define (f64-deserialize sexp)
    (okm-match sexp (':f64 (make-f64))))

  (define-record-type (ptr make-ptr ptr?))
  (define (ptr-serialize ptr)
    (okm-assert (ptr? ptr))
    ':ptr)
  (define (ptr-deserialize sexp)
    (okm-match sexp (':ptr (make-ptr))))

  ;; Function Type
  (define-record-type (func-type make-func-type func-type?)
    (fields
      (immutable param-types func-type-param-types)
      (immutable return-types func-type-return-types)))

  (define (parse-type-list sexp)
    (if (null? sexp)
        '()
        (if (list? sexp)
            (let ((ts (map deserialize-type sexp)))
              (if (for-all core-type? ts)
                  ts
                  (let ((single (deserialize-type sexp)))
                    (if single (list single) #f))))
            (let ((single (deserialize-type sexp)))
              (if single (list single) #f)))))

  (define (func-type-serialize fn-ty)
    (okm-assert (func-type? fn-ty))
    (let ((params-sexp (map serialize-type (func-type-param-types fn-ty)))
          (returns-sexp (map serialize-type (func-type-return-types fn-ty))))
      `(func ,params-sexp -> ,returns-sexp)))

  (define (func-type-deserialize sexp)
    (okm-match sexp
      (('func params-sexp '-> rets-sexp)
       (let ((param-types (parse-type-list params-sexp))
             (ret-types (parse-type-list rets-sexp)))
         (okm-assert-guard
           (param-types
            ret-types)
           (make-func-type param-types ret-types))))))

  ;; Core Type Registry
  (define *core-type-predicates* (list i32? i64? f32? f64? ptr? func-type?))
  (define *core-type-deserializers* (list i32-deserialize i64-deserialize f32-deserialize f64-deserialize ptr-deserialize func-type-deserialize))
  (define *core-type-serializers* (list (cons i32? i32-serialize) (cons i64? i64-serialize) (cons f32? f32-serialize) (cons f64? f64-serialize) (cons ptr? ptr-serialize) (cons func-type? func-type-serialize)))

  (define (core-type? obj)
    (exists (lambda (pred) (pred obj)) *core-type-predicates*))

  (define (serialize-type obj)
    (okm-assert (core-type? obj))
    (let loop ((entries *core-type-serializers*))
      (if (null? entries)
          #f
          (let ((pred (caar entries))
                (ser (cdar entries)))
            (if (pred obj)
                (ser obj)
                (loop (cdr entries)))))))

  (define (deserialize-type sexp)
    (let loop ((desers *core-type-deserializers*))
      (if (null? desers)
          #f
          (or ((car desers) sexp)
              (loop (cdr desers))))))

  ;; Register
  (define-record-type (register make-register register?)
    (fields
      (immutable name register-name)
      (immutable type register-type)))

  (define (valid-register-name? reg)
    (and (symbol? reg)
         (let ((str (symbol->string reg)))
           (and (> (string-length str) 1)
                (char=? (string-ref str 0) #\%)))))

  ;; Symbol
  (define-record-type (okm-symbol make-okm-symbol okm-symbol?)
    (fields
      (immutable name okm-symbol-name)
      (immutable type okm-symbol-type)
      (immutable def okm-symbol-def)))

  (define (okm-valid-symbol-name? sym)
    (and (symbol? sym)
         (let ((str (symbol->string sym)))
           (and (> (string-length str) 1)
                (char=? (string-ref str 0) #\$)))))

  ;; Region
  (define-record-type (region make-region region?)
    (fields
      (immutable blocks region-blocks)))

  (define (region-deserialize lst)
    (okm-match lst
      (('region . blocks-sexp)
       (let ((blocks (map block-deserialize blocks-sexp)))
         (okm-assert-guard
           ((for-all block? blocks))
           (make-region blocks))))))

  ;; Block
  (define-record-type (block make-block block?)
    (fields
      (immutable name block-name)
      (immutable instructions block-instructions)))

  (define block-ops block-instructions)

  (define (block-deserialize lst)
    (okm-match lst
      (('block name . ops-sexp)
       (let ((ops (map read-instruction ops-sexp)))
         (okm-assert-guard
           ((for-all instruction? ops))
           (make-block name ops))))))

  ;; Instruction
  (define-record-type (instruction make-instruction instruction?)
    (fields
      (immutable op-type instruction-op-type)
      (immutable op instruction-op)
      (immutable target instruction-target)
      (immutable attributes instruction-attributes)))

  (define (split-at-symbol lst sym)
    (cond
      ((null? lst) (values '() lst))
      ((eq? (car lst) sym) (values '() lst))
      (else (let-values (((front rest) (split-at-symbol (cdr lst) sym)))
              (values (cons (car lst) front) rest)))))

  (define (parse-target-front front)
    (if (= (length front) 2)
        (let ((reg (car front))
              (ty-sexp (cadr front)))
          (let ((t (deserialize-type ty-sexp)))
            (if (and (valid-register-name? reg) t)
                (make-register reg t)
                #f)))
        #f))

  (define (read-instruction lst)
    (if (not (and (list? lst) (not (null? lst))))
        (begin (okm-assert #f) #f)
        (let-values (((front rest) (split-at-symbol lst '=)))
          (if (null? rest)
              ;; No target
              (cond
                ((symbol? (car front))
                 (let* ((op-part front)
                        (op-type (car op-part))
                        (op (deserialize-op op-part)))
                   (okm-assert-guard
                     (op)
                     (make-instruction op-type op #f '()))))
                ((and (pair? (car front)) (symbol? (caar front)))
                 (let* ((op-part (car front))
                        (op-type (car op-part))
                        (op (deserialize-op op-part))
                        (attributes (cdr front)))
                   (okm-assert-guard
                     (op)
                     (make-instruction op-type op #f attributes))))
                (else
                 (okm-assert #f)
                 #f))
              ;; With target
              (let* ((instruction-parts (cdr rest))
                     (op-part (car instruction-parts))
                     (op-type (and (pair? op-part) (car op-part)))
                     (op (and op-type (deserialize-op op-part)))
                     (attributes (cdr instruction-parts)))
                (let ((target-reg (parse-target-front front)))
                  (okm-assert-guard
                    (op
                     target-reg)
                    (make-instruction op-type op target-reg attributes))))))))

  (define (instruction-serialize op)
    (okm-assert (instruction? op))
    (let* ((op-type (instruction-op-type op))
           (inner-op (instruction-op op))
           (serialized-op (serialize-op op-type inner-op))
           (target (instruction-target op))
           (attributes (instruction-attributes op)))
      (if (not target)
          (if (null? attributes)
              serialized-op
              (cons serialized-op attributes))
          (let ((target-parts (list (register-name target)
                                    (serialize-type (register-type target))))
                (op-part (cons serialized-op attributes)))
            (append target-parts (cons '= op-part))))))

  (define (block-serialize block)
    (okm-assert (block? block))
    (cons 'block
          (cons (block-name block)
                (map instruction-serialize (block-instructions block)))))

  (define (region-serialize region)
    (okm-assert (region? region))
    (cons 'region
          (map block-serialize (region-blocks region))))
)
