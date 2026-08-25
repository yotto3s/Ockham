(library (ockham core)
  (export
    register-op unregister-op serialize-op deserialize-op
    define-dialect-op

    int make-int int?
    int-size int-serialize int-deserialize

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
    block-name block-ops
    block-serialize block-deserialize

    operation make-operation operation?
    operation-op-type operation-op operation-targets
    operation-attributes
    read-operation operation-serialize

    abi make-abi abi?
    abi-general-registers abi-caller-saved abi-callee-saved abi-subregisters
    abi-argument-registers abi-return-registers abi-sp-register abi-fp-register
    abi-caller-saved-register? abi-callee-saved-register? abi-argument-register?
    abi-argument-register? abi-return-register?

    target make-target target?
    target-arch target-os target-abi target-constraints

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

  ;; Operation
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

  ;; okm Types
  ;; Integer
  (define-record-type (int make-int int?)
    (fields
      (immutable size int-size)))

  (define (int-serialize okm)
    (okm-assert (int? okm))
    `(int ,(int-size okm)))

  (define (int-deserialize lst)
    (okm-match lst
      (('int size)
       (okm-assert-guard
         ((integer? size)
          (> size 0))
         (make-int size)))))

  ;; Pointer
  (define-record-type (ptr make-ptr ptr?))
  (define (ptr-serialize ptr)
    (okm-assert (ptr? ptr))
    'ptr)

  (define (ptr-deserialize p)
    (okm-match p
      ('ptr (make-ptr))))

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
  (define *core-type-predicates* (list int? ptr? func-type?))
  (define *core-type-deserializers* (list int-deserialize ptr-deserialize func-type-deserialize))
  (define *core-type-serializers* (list (cons int? int-serialize) (cons ptr? ptr-serialize) (cons func-type? func-type-serialize)))

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
      (immutable ops block-ops)))

  (define (block-deserialize lst)
    (okm-match lst
      (('block name . ops-sexp)
       (let ((ops (map read-operation ops-sexp)))
         (okm-assert-guard
           ((for-all operation? ops))
           (make-block name ops))))))

  ;; Operation
  (define-record-type (operation make-operation operation?)
    (fields
      (immutable op-type operation-op-type)
      (immutable op operation-op)
      (immutable targets operation-targets)
      (immutable attributes operation-attributes)))

  (define (split-at-symbol lst sym)
    (cond
      ((null? lst) (values '() lst))
      ((eq? (car lst) sym) (values '() lst))
      (else (let-values (((front rest) (split-at-symbol (cdr lst) sym)))
              (values (cons (car lst) front) rest)))))

  (define (read-operation lst)
    (if (not (and (list? lst) (not (null? lst))))
        (begin (okm-assert #f) #f)
        (let-values (((front rest) (split-at-symbol lst '=)))
          (if (null? rest)
              ;; No targets
              (cond
                ((symbol? (car front))
                 (let* ((op-part front)
                        (op-type (car op-part))
                        (op (deserialize-op op-part)))
                   (okm-assert-guard
                     (op)
                     (make-operation op-type op '() '()))))
                ((and (pair? (car front)) (symbol? (caar front)))
                 (let* ((op-part (car front))
                        (op-type (car op-part))
                        (op (deserialize-op op-part))
                        (attributes (cdr front)))
                   (okm-assert-guard
                     (op)
                     (make-operation op-type op '() attributes))))
                (else
                 (okm-assert #f)
                 #f))
              ;; With targets
              (let-values (((names-part rest-colon) (split-at-symbol front ':)))
                (if (null? rest-colon)
                    (begin (okm-assert #f) #f)
                    (let* ((types-part (cdr rest-colon))
                           (types (map deserialize-type types-part))
                           (operation-parts (cdr rest))
                           (op-part (car operation-parts))
                           (op-type (and (pair? op-part) (car op-part)))
                           (op (and op-type (deserialize-op op-part)))
                           (attributes (cdr operation-parts)))
                      (okm-assert-guard
                        (op
                         (for-all valid-register-name? names-part)
                         (for-all core-type? types)
                         (= (length names-part) (length types)))
                        (let ((registers (map make-register names-part types)))
                          (make-operation op-type op registers attributes))))))))))

  (define (operation-serialize op)
    (okm-assert (operation? op))
    (let* ((op-type (operation-op-type op))
           (inner-op (operation-op op))
           (serialized-op (serialize-op op-type inner-op))
           (targets (operation-targets op))
           (attributes (operation-attributes op)))
      (if (null? targets)
          (if (null? attributes)
              serialized-op
              (cons serialized-op attributes))
          (let ((names-part (map register-name targets))
                (types-part (map (lambda (r) (serialize-type (register-type r))) targets))
                (op-part (cons serialized-op attributes)))
            (append names-part (cons ': (append types-part (cons '= op-part))))))))

  (define (block-serialize block)
    (okm-assert (block? block))
    (cons 'block
          (cons (block-name block)
                (map operation-serialize (block-ops block)))))

  (define (region-serialize region)
    (okm-assert (region? region))
    (cons 'region
          (map block-serialize (region-blocks region))))

  (define-record-type (abi make-abi abi?)
    (fields
      (immutable general-registers abi-general-registers)
      (immutable caller-saved abi-caller-saved)
      (immutable callee-saved abi-callee-saved)
      (immutable argument-registers abi-argument-registers)
      (immutable return-registers abi-return-registers)
      (immutable subregisters abi-subregisters)
      (immutable sp-register abi-sp-register)
      (immutable fp-register abi-fp-register)))

  (define-syntax define-register-predicate
    (syntax-rules ()
      ((_ funname accessor)
       (define (funname abi reg)
         (not (not (memq reg (accessor abi))))))))

  (define-syntax define-register-predicates
    (syntax-rules ()
      ((_ (funname acc) ...)
       (begin (define-register-predicate funname acc) ...))))

  (define-register-predicates
    (abi-caller-saved-register? abi-caller-saved)
    (abi-callee-saved-register? abi-callee-saved)
    (abi-argument-register? abi-argument-registers)
    (abi-return-register? abi-return-registers))

  (define-record-type (target make-target target?)
    (fields
      (immutable arch target-arch)
      (immutable os target-os)
      (immutable abi target-abi)
      (immutable constraints target-constraints)))

  ;; Macro for defining dialect operators
  (define-syntax define-dialect-op
    (lambda (stx)
      (define (symbolic-append k . args)
        (datum->syntax k
          (string->symbol
            (apply string-append
                   (map (lambda (x)
                          (cond
                            ((symbol? x) (symbol->string x))
                            ((string? x) x)
                            (else (symbol->string (syntax->datum x)))))
                        args)))))

      (define (parse-op-spec op-spec-stx)
        (syntax-case op-spec-stx ()
          ((op-name make-name pred-name)
           (values #'op-name #'make-name #'pred-name))
          ((op-name make-name)
           (values #'op-name #'make-name (symbolic-append #'op-name #'op-name "?")))
          (op-name
           (values #'op-name
                   (symbolic-append #'op-name "make-" #'op-name)
                   (symbolic-append #'op-name #'op-name "?")))))

      (syntax-case stx ()
        ((_ (dialect op-spec)
            (kw-f field-spec ...)
            (kw-s ser-expr)
            (kw-d deser-expr))
         (and (eq? (syntax->datum #'kw-f) 'fields)
              (eq? (syntax->datum #'kw-s) 'serializer)
              (eq? (syntax->datum #'kw-d) 'deserializer))
         (let-values (((op-name make-name pred-name) (parse-op-spec #'op-spec)))
           (let* ((d-sym (syntax->datum #'dialect))
                  (op-s (syntax->datum op-name))
                  (full-sym (string->symbol (string-append (symbol->string d-sym) ":" (symbol->string op-s)))))
             (with-syntax
               ((op-name op-name)
                (make-name make-name)
                (pred-name pred-name)
                (op-sym (datum->syntax op-name full-sym))
                (ser-name (symbolic-append op-name op-name "-serialize"))
                (deser-name (symbolic-append op-name op-name "-deserialize")))
               (letrec
                 ((transform-ser
                    (lambda (stx-in)
                      (syntax-case stx-in (quasiquote list quote)
                        ((quasiquote (_ . rest))
                         #`(quasiquote (op-sym . rest)))
                        ((list (quote _) . rest)
                         #`(list (quote op-sym) . rest))
                        ((list _ . rest)
                         #`(list (quote op-sym) . rest))
                        ((head . tail)
                         #`(#,(transform-ser #'head) . #,(transform-ser #'tail)))
                        (other #'other))))
                  (transform-deser-clause
                    (lambda (clause-stx)
                      (syntax-case clause-stx ()
                        (((head . pat-rest) body ...)
                         (eq? (syntax->datum #'head) '_)
                         #`(((quote op-sym) . pat-rest) body ...))
                        (other #'other))))
                  (transform-deser
                    (lambda (stx-in)
                      (syntax-case stx-in (match okm-match)
                        ((match lst clause ...)
                         (with-syntax (((tc ...) (map transform-deser-clause #'(clause ...))))
                           #`(match lst tc ...)))
                        ((okm-match lst clause ...)
                         (with-syntax (((tc ...) (map transform-deser-clause #'(clause ...))))
                           #`(match lst tc ... (_ #f))))
                        ((head . tail)
                         #`(#,(transform-deser #'head) . #,(transform-deser #'tail)))
                        (other #'other)))))
                 (with-syntax
                   ((transformed-ser (transform-ser #'ser-expr))
                    (transformed-deser (transform-deser #'deser-expr)))
                (with-syntax
                  ((ser-def
                     (if (and (identifier? #'ser-expr)
                              (eq? (syntax->datum #'ser-expr) (syntax->datum #'ser-name)))
                         #'(begin)
                         #'(define ser-name transformed-ser)))
                   (deser-def
                     (if (and (identifier? #'deser-expr)
                              (eq? (syntax->datum #'deser-expr) (syntax->datum #'deser-name)))
                         #'(begin)
                         #'(define deser-name transformed-deser)))
                   (actual-ser-proc (if (identifier? #'ser-expr) #'ser-expr #'ser-name))
                   (actual-deser-proc (if (identifier? #'deser-expr) #'deser-expr #'deser-name))
                   (reg-dummy (symbolic-append #'op-name #'op-name "-reg-dummy"))
                   (raw-pred-name (symbolic-append #'op-name #'op-name "-raw-pred?")))
                  #'(begin
                      (define-record-type (op-name make-name raw-pred-name)
                        (fields field-spec ...))
                      ser-def
                      deser-def
                      (define reg-dummy (register-op 'op-sym actual-ser-proc actual-deser-proc))
                      (define (pred-name obj)
                        (and reg-dummy (raw-pred-name obj))))))))))))))
)
