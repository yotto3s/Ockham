(library (ockham core)
  (export
    register-op unregister-op serialize-op deserialize-op

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
    target-arch target-os target-abi target-constraints)
  (import (rnrs (6))
          (ufo-match))

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

  (define (deserialize-op op-type arguments)
    (let ((entry (assoc op-type *op-types*)))
      (if entry
        ((caddr entry) arguments)
        #f)))

  ;; okm Types
  ;; Integer
  (define-record-type (int make-int int?)
    (fields
      (immutable size int-size)))

  (define (int-serialize okm) `(int ,(int-size okm)))

  (define (int-deserialize lst)
    (match lst
      (('int size) (make-int size))
      (_ #f)))

  ;; Pointer
  (define-record-type (ptr make-ptr ptr?))
  (define (ptr-serialize ptr) 'ptr)
  (define (ptr-deserialize p)
    (if (eq? p 'ptr)
      (make-ptr)
      #f))

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
       (if (eq? (car lst) 'region)
         (let ((region (make-region #f parent)))
           (set-region-blocks!
             region (map (lambda (block) (block-deserialize block region)) (cdr lst)))
           region)
         #f))))


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
       (if (eq? (car lst) 'block)
         (let* ((name (cadr lst))
                (block (make-block name #f parent)))
           (set-block-ops!
             block (map (lambda (op) (read-operation op block)) (cddr lst)))
           block)
         #f))))

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
        (let* ((op-type (caar operation))
               (arguments (cdar operation))
               (op (deserialize-op op-type arguments))
               (attributes (cdr operation)))
           (make-operation op-type op targets attributes parent)))))

  (define (operation-serialize op)
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
    (cons 'block
          (cons (block-name block)
                (map operation-serialize (block-ops block)))))

  (define (region-serialize region)
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
)
