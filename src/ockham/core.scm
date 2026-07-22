(library (ockham core)
  (export
    register-op unregister-op serialize-op deserialize-op
    define-dialect-op

    int make-int int?
    int-size int-serialize int-deserialize

    ptr make-ptr ptr?
    ptr-serialize ptr-deserialize

    register make-register register?
    register-name register-type register-def
    valid-register-name?

    okm-symbol make-okm-symbol okm-symbol?
    okm-symbol-name okm-symbol-type okm-symbol-def
    okm-valid-symbol-name?

    region make-region region?
    region-blocks set-region-blocks! region-parent set-region-parent!
    region-serialize region-deserialize

    block make-block block?
    block-name block-ops set-block-ops! block-parent set-block-parent!
    block-serialize block-deserialize

    operation make-operation operation?
    operation-op-type operation-op operation-targets
    operation-attributes operation-parent
    read-operation operation-serialize

    abi make-abi abi?
    abi-general-registers abi-caller-saved abi-callee-saved
    abi-sp-register abi-fp-register

    target make-target target?
    target-arch target-os target-abi target-constraints

    log-error okm-assert okm-assert-guard error-count error-messages reset-error-log!
    core-type? register-core-type unregister-core-type serialize-type deserialize-type)
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
    (match lst
      (('int size)
       (okm-assert-guard
         ((integer? size)
          (> size 0))
         (make-int size)))
      (_ #f)))

  ;; Pointer
  (define-record-type (ptr make-ptr ptr?))
  (define (ptr-serialize ptr)
    (okm-assert (ptr? ptr))
    'ptr)

  (define (ptr-deserialize p)
    (match p
      ('ptr (make-ptr))
      (_ #f)))

  ;; Core Type Registry
  (define *core-type-predicates* (list int? ptr?))
  (define *core-type-deserializers* (list int-deserialize ptr-deserialize))
  (define *core-type-serializers* (list (cons int? int-serialize) (cons ptr? ptr-serialize)))

  (define (register-core-type pred serializer deserializer)
    (set! *core-type-predicates* (cons pred *core-type-predicates*))
    (set! *core-type-serializers* (cons (cons pred serializer) *core-type-serializers*))
    (set! *core-type-deserializers* (cons deserializer *core-type-deserializers*)))

  (define (unregister-core-type pred)
    (set! *core-type-predicates* (remp (lambda (p) (eq? p pred)) *core-type-predicates*))
    (set! *core-type-serializers* (remp (lambda (entry) (eq? (car entry) pred)) *core-type-serializers*))
    (set! *core-type-deserializers* (remp (lambda (d) (eq? d pred)) *core-type-deserializers*)))

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
      (immutable type register-type)
      (immutable def register-def)))

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
                (char=? (string-ref str 0) #\@)))))

  ;; Region
  (define-record-type (region make-region region?)
    (fields
      (mutable blocks region-blocks set-region-blocks!)
      (mutable parent region-parent set-region-parent!)))

  (define region-deserialize
    (case-lambda
      ((lst) (region-deserialize lst #f))
      ((lst parent)
       (match lst
         (('region . blocks-sexp)
          (let ((region (make-region #f parent)))
            (let ((blocks (map (lambda (block) (block-deserialize block region)) blocks-sexp)))
              (okm-assert-guard
                ((for-all block? blocks))
                (begin
                  (set-region-blocks! region blocks)
                  region)))))
         (_ #f)))))


  ;; Block
  (define-record-type (block make-block block?)
    (fields
      (immutable name block-name)
      (mutable ops block-ops set-block-ops!)
      (mutable parent block-parent set-block-parent!)))

  (define block-deserialize
    (case-lambda
      ((lst) (block-deserialize lst #f))
      ((lst parent)
       (match lst
         (('block name . ops-sexp)
          (let ((block (make-block name #f parent)))
            (let ((ops (map (lambda (op) (read-operation op block)) ops-sexp)))
              (okm-assert-guard
                ((for-all operation? ops))
                (begin
                  (set-block-ops! block ops)
                  block)))))
         (_ #f)))))

  ;; Operation
  (define-record-type (operation make-operation operation?)
    (fields
      (immutable op-type operation-op-type)
      (immutable op operation-op)
      (immutable targets operation-targets)
      (immutable attributes operation-attributes)
      (immutable parent operation-parent)))

  (define (split-at-symbol lst sym)
    (cond
      ((null? lst) (values '() lst))
      ((eq? (car lst) sym) (values '() lst))
      (else (let-values (((front rest) (split-at-symbol (cdr lst) sym)))
              (values (cons (car lst) front) rest)))))

  (define (read-operation lst parent)
    (let-values (((front rest) (split-at-symbol lst '=)))
      (let-values (((targets operation)
        (if (null? rest)
          (values '() front)
          (values front (cdr rest)))))
        (let* ((op-part (car operation))
               (op-type (car op-part))
               (op (deserialize-op op-part))
               (attributes (cdr operation)))
          (okm-assert-guard
            (op)
            (make-operation op-type op targets attributes parent))))))

  (define (operation-serialize op)
    (okm-assert (operation? op))
    (let* ((op-type (operation-op-type op))
           (inner-op (operation-op op))
           (serialized-op (serialize-op op-type inner-op))
           (targets (operation-targets op))
           (attributes (operation-attributes op))
           (op-part (cons serialized-op attributes)))
      (if (null? targets)
          op-part
          (append targets (cons '= op-part)))))

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
      (immutable sp-register abi-sp-register)
      (immutable fp-register abi-fp-register)))

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
                      (syntax-case stx-in (match)
                        ((match lst clause ...)
                         (with-syntax (((tc ...) (map transform-deser-clause #'(clause ...))))
                           #`(match lst tc ...)))
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
                   (reg-dummy (symbolic-append #'op-name #'op-name "-reg-dummy")))
                  #'(begin
                      (define-record-type (op-name make-name pred-name)
                        (fields field-spec ...))
                      ser-def
                      deser-def
                      (define reg-dummy (register-op 'op-sym actual-ser-proc actual-deser-proc)))))))))))))
)
