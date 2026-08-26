(library (ockham pass-manager)
  (export
    pass make-pass pass?
    pass-name pass-proc pass-description
    define-pass

    pass-manager make-pass-manager pass-manager?
    pass-manager-passes
    pass-manager-add-pass!
    pass-manager-run
    run-pass

    module-map-blocks
    module-map-instructions)
  (import (rnrs (6))
          (ockham core)
          (ockham ops))

  (define-record-type (pass %make-pass pass?)
    (fields
      (immutable name pass-name)
      (immutable proc pass-proc)
      (immutable description pass-description)))

  (define make-pass
    (case-lambda
      ((name proc) (%make-pass name proc ""))
      ((name proc description) (%make-pass name proc description))))

  (define-syntax define-pass
    (lambda (stx)
      (syntax-case stx ()
        ((_ name (arg) doc body ...)
         (string? (syntax->datum #'doc))
         #'(define name
             (make-pass 'name
                        (lambda (arg) body ...)
                        doc)))
        ((_ name (arg) body ...)
         #'(define name
             (make-pass 'name
                        (lambda (arg) body ...)))))))

  (define-record-type (pass-manager %make-pass-manager pass-manager?)
    (fields
      (mutable passes %pass-manager-passes set-pass-manager-passes!)))

  (define (pass-manager-passes pm)
    (okm-assert (pass-manager? pm))
    (%pass-manager-passes pm))

  (define make-pass-manager
    (case-lambda
      (() (%make-pass-manager '()))
      ((passes)
       (okm-assert (and (list? passes) (for-all pass? passes)))
       (%make-pass-manager passes))))

  (define (pass-manager-add-pass! pm pass-obj)
    (okm-assert (pass-manager? pm))
    (okm-assert (pass? pass-obj))
    (set-pass-manager-passes! pm (append (%pass-manager-passes pm) (list pass-obj))))

  (define (run-pass p module-obj)
    (okm-assert (pass? p))
    (okm-assert (module? module-obj))
    ((pass-proc p) module-obj))

  (define (pass-manager-run pm module-obj)
    (okm-assert (pass-manager? pm))
    (okm-assert (module? module-obj))
    (let loop ((passes (%pass-manager-passes pm))
               (curr module-obj))
      (if (null? passes)
          curr
          (loop (cdr passes) (run-pass (car passes) curr)))))

  (define (module-map-blocks proc mod)
    (okm-assert (module? mod))
    (let* ((body (module-body mod))
           (blocks (region-blocks body))
           (new-blocks (map proc blocks)))
      (make-module (module-name mod) (make-region new-blocks))))

  (define (module-map-instructions proc mod)
    (okm-assert (module? mod))
    (module-map-blocks
      (lambda (blk)
        (let* ((insts (block-instructions blk))
               (new-insts (map proc insts)))
          (make-block (block-name blk) new-insts)))
      mod))
)
