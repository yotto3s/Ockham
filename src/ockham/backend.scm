(library (ockham backend)
  (export constant make-constant constant?
          constant-type constant-value
          constant-serialize constant-deserialize

          copy make-copy copy?
          copy-type copy-operand
          copy-serialize copy-deserialize

          add make-add add?
          add-type add-lhs add-rhs
          add-serialize add-deserialize

          sub make-sub sub?
          sub-type sub-lhs sub-rhs
          sub-serialize sub-deserialize

          mul make-mul mul?
          mul-type mul-lhs mul-rhs
          mul-serialize mul-deserialize

          idiv make-idiv idiv?
          idiv-type idiv-lhs idiv-rhs
          idiv-serialize idiv-deserialize

          udiv make-udiv udiv?
          udiv-type udiv-lhs udiv-rhs
          udiv-serialize udiv-deserialize

          lshift make-lshift lshift?
          lshift-type lshift-lhs lshift-rhs
          lshift-serialize lshift-deserialize

          rshift make-rshift rshift?
          rshift-type rshift-lhs rshift-rhs
          rshift-serialize rshift-deserialize

          irem make-irem irem?
          irem-type irem-lhs irem-rhs
          irem-serialize irem-deserialize

          urem make-urem urem?
          urem-type urem-lhs urem-rhs
          urem-serialize urem-deserialize

          sext make-sext sext?
          sext-type sext-operand
          sext-serialize sext-deserialize

          zext make-zext zext?
          zext-type zext-operand
          zext-serialize zext-deserialize

          load make-load load?
          load-type load-ptr load-offset
          load-serialize load-deserialize

          store make-store store?
          store-type store-ptr store-val store-offset
          store-serialize store-deserialize

          jmp make-jmp jmp?
          jmp-target
          jmp-serialize jmp-deserialize

          br-cond make-br-cond br-cond?
          br-cond-condition br-cond-then-target br-cond-else-target
          br-cond-serialize br-cond-deserialize

          syscall make-syscall syscall?
          syscall-id syscall-args
          syscall-serialize syscall-deserialize

          func make-func func?
          func-name func-args func-return-types func-body
          func-serialize func-deserialize)
  (import (rnrs (6))
          (ufo-match)
          (ockham core))

  ; TODO: Impelement core operators
  (define backend-operators
    '(constant copy add sub mul idiv udiv lshift rshift irem urem sext zext
      load store jmp br-cond syscall call ret func global-int global-bytes module extern))

  (define-dialect-op (be constant)
    (fields
      (immutable type constant-type)
      (immutable value constant-value))
    (serializer
      (lambda (op)
        `(_ ,(constant-value op) : ,(serialize-type (constant-type op)))))
    (deserializer
      (lambda (lst)
        (match lst
          ((_ value ': ty)
           (let ((t (deserialize-type ty)))
             (okm-assert-guard (t) (make-constant t value))))
          (_ #f)))))

  (define-dialect-op (be copy)
    (fields
      (immutable type copy-type)
      (immutable operand copy-operand))
    (serializer
      (lambda (op)
        (okm-assert (valid-register-name? (copy-operand op)))
        `(_ ,(copy-operand op) : ,(serialize-type (copy-type op)))))
    (deserializer
      (lambda (lst)
        (match lst
          ((_ operand ': ty)
           (let ((t (deserialize-type ty)))
             (okm-assert-guard
               (t
                (valid-register-name? operand))
               (make-copy t operand))))
          (_ #f)))))

  (define (int-or-ptr? t) (or (int? t) (ptr? t)))

  (define-syntax define-binary-op
    (lambda (stx)
      (syntax-case stx ()
        ((_ name type-pred)
         (let* ((name-sym (syntax->datum #'name))
                (str (symbol->string name-sym))
                (make-sym (string->symbol (string-append "make-" str)))
                (type-sym (string->symbol (string-append str "-type")))
                (lhs-sym (string->symbol (string-append str "-lhs")))
                (rhs-sym (string->symbol (string-append str "-rhs"))))
           (with-syntax ((make-op (datum->syntax #'name make-sym))
                         (op-type (datum->syntax #'name type-sym))
                         (op-lhs (datum->syntax #'name lhs-sym))
                         (op-rhs (datum->syntax #'name rhs-sym)))
             #'(define-dialect-op (be name)
                 (fields
                   (immutable type op-type)
                   (immutable lhs op-lhs)
                   (immutable rhs op-rhs))
                 (serializer
                   (lambda (op)
                     (okm-assert (type-pred (op-type op)))
                     (okm-assert (valid-register-name? (op-lhs op)))
                     (okm-assert (valid-register-name? (op-rhs op)))
                     `(_ ,(op-lhs op) ,(op-rhs op) : ,(serialize-type (op-type op)))))
                 (deserializer
                   (lambda (lst)
                     (match lst
                       ((_ lhs rhs ': ty)
                        (let ((t (deserialize-type ty)))
                          (okm-assert-guard
                            ((and t (type-pred t))
                             (valid-register-name? lhs)
                             (valid-register-name? rhs))
                            (make-op t lhs rhs))))
                       (_ #f)))))))))))

  (define-syntax define-binary-ops
    (syntax-rules ()
      ((_ (op pred) ...)
       (begin (define-binary-op op pred) ...))))

  (define-syntax define-extension-op
    (lambda (stx)
      (syntax-case stx ()
        ((_ name)
         (let* ((name-sym (syntax->datum #'name))
                (str (symbol->string name-sym))
                (make-sym (string->symbol (string-append "make-" str)))
                (type-sym (string->symbol (string-append str "-type")))
                (operand-sym (string->symbol (string-append str "-operand"))))
           (with-syntax ((make-op (datum->syntax #'name make-sym))
                         (op-type (datum->syntax #'name type-sym))
                         (op-operand (datum->syntax #'name operand-sym)))
             #'(define-dialect-op (be name)
                 (fields
                   (immutable type op-type)
                   (immutable operand op-operand))
                 (serializer
                   (lambda (op)
                     (okm-assert (int? (op-type op)))
                     (okm-assert (valid-register-name? (op-operand op)))
                     `(_ ,(op-operand op) : ,(serialize-type (op-type op)))))
                 (deserializer
                   (lambda (lst)
                     (match lst
                       ((_ operand ': ty)
                        (let ((t (deserialize-type ty)))
                          (okm-assert-guard
                            ((and t (int? t))
                             (valid-register-name? operand))
                            (make-op t operand))))
                       (_ #f)))))))))))

  (define-binary-ops
    (add int-or-ptr?)
    (sub int-or-ptr?)
    (mul int?)
    (idiv int?)
    (udiv int?)
    (lshift int?)
    (rshift int?)
    (irem int?)
    (urem int?))

  (define-extension-op sext)
  (define-extension-op zext)

  (define-dialect-op (be load)
    (fields
      (immutable type load-type)
      (immutable ptr load-ptr)
      (immutable offset load-offset))
    (serializer
      (lambda (op)
        (okm-assert (valid-register-name? (load-ptr op)))
        (if (and (load-offset op) (not (zero? (load-offset op))))
            `(_ ,(load-ptr op) ,(load-offset op) : ,(serialize-type (load-type op)))
            `(_ ,(load-ptr op) : ,(serialize-type (load-type op))))))
    (deserializer
      (lambda (lst)
        (match lst
          ((_ ptr offset ': ty)
           (let ((t (deserialize-type ty)))
             (okm-assert-guard
               (t
                (valid-register-name? ptr))
               (make-load t ptr offset))))
          ((_ ptr ': ty)
           (let ((t (deserialize-type ty)))
             (okm-assert-guard
               (t
                (valid-register-name? ptr))
               (make-load t ptr 0))))
          (_ #f)))))

  (define-dialect-op (be store)
    (fields
      (immutable type store-type)
      (immutable ptr store-ptr)
      (immutable val store-val)
      (immutable offset store-offset))
    (serializer
      (lambda (op)
        (okm-assert (valid-register-name? (store-ptr op)))
        (okm-assert (valid-register-name? (store-val op)))
        (if (and (store-offset op) (not (zero? (store-offset op))))
            `(_ ,(store-ptr op) ,(store-val op) ,(store-offset op) : ,(serialize-type (store-type op)))
            `(_ ,(store-ptr op) ,(store-val op) : ,(serialize-type (store-type op))))))
    (deserializer
      (lambda (lst)
        (match lst
          ((_ ptr val offset ': ty)
           (let ((t (deserialize-type ty)))
             (okm-assert-guard
               (t
                (valid-register-name? ptr)
                (valid-register-name? val))
               (make-store t ptr val offset))))
          ((_ ptr val ': ty)
           (let ((t (deserialize-type ty)))
             (okm-assert-guard
               (t
                (valid-register-name? ptr)
                (valid-register-name? val))
               (make-store t ptr val 0))))
          (_ #f)))))

  (define-dialect-op (be jmp)
    (fields
      (immutable target jmp-target))
    (serializer
      (lambda (op)
        (let ((tgt (jmp-target op)))
          (when (and (pair? tgt) (pair? (cdr tgt)))
            (for-each (lambda (arg) (okm-assert (valid-register-name? arg))) (cdr tgt)))
          `(_ ,tgt))))
    (deserializer
      (lambda (lst)
        (match lst
          ((_ target)
           (okm-assert-guard
             ((pair? target)
              (or (null? (cdr target))
                  (for-all valid-register-name? (cdr target))))
             (make-jmp target)))
          (_ #f)))))

  (define-dialect-op (be br-cond)
    (fields
      (immutable condition br-cond-condition)
      (immutable then-target br-cond-then-target)
      (immutable else-target br-cond-else-target))
    (serializer
      (lambda (op)
        (okm-assert (valid-register-name? (br-cond-condition op)))
        (let ((then-t (br-cond-then-target op))
              (else-t (br-cond-else-target op)))
          (when (and (pair? then-t) (pair? (cdr then-t)))
            (for-each (lambda (arg) (okm-assert (valid-register-name? arg))) (cdr then-t)))
          (when (and (pair? else-t) (pair? (cdr else-t)))
            (for-each (lambda (arg) (okm-assert (valid-register-name? arg))) (cdr else-t)))
          `(_ ,(br-cond-condition op) ,then-t ,else-t))))
    (deserializer
      (lambda (lst)
        (match lst
          ((_ condition then-target else-target)
           (okm-assert-guard
             ((valid-register-name? condition)
              (pair? then-target)
              (pair? else-target)
              (or (null? (cdr then-target)) (for-all valid-register-name? (cdr then-target)))
              (or (null? (cdr else-target)) (for-all valid-register-name? (cdr else-target))))
             (make-br-cond condition then-target else-target)))
          (_ #f)))))

  (define-dialect-op (be syscall)
    (fields
      (immutable id syscall-id)
      (immutable args syscall-args))
    (serializer
      (lambda (op)
        (okm-assert (integer? (syscall-id op)))
        (let ((args (syscall-args op)))
          (okm-assert (and (list? args) (<= (length args) 6)))
          (for-each (lambda (arg) (okm-assert (valid-register-name? arg))) args)
          `(_ ,(syscall-id op) . ,args))))
    (deserializer
      (lambda (lst)
        (match lst
          ((_ id . args)
           (okm-assert-guard
             ((integer? id)
              (list? args)
              (<= (length args) 6)
              (for-all valid-register-name? args))
             (make-syscall id args)))
          (_ #f)))))

  (define (func-build-sexp op)
    (okm-assert (okm-valid-symbol-name? (func-name op)))
    (let ((args-sexp (map (lambda (a)
                            (okm-assert (valid-register-name? (car a)))
                            (list (car a) ': (serialize-type (cdr a))))
                          (func-args op)))
          (rets-sexp (map serialize-type (func-return-types op)))
          (body-sexp (region-serialize (func-body op))))
      (list 'be:func (func-name op) args-sexp '-> rets-sexp body-sexp)))

  (define-dialect-op (be func)
    (fields
      (immutable name func-name)
      (immutable args func-args)
      (immutable return-types func-return-types)
      (immutable body func-body))
    (serializer func-build-sexp)
    (deserializer
      (lambda (lst)
        (match lst
          ((_ name args-sexp '-> rets-sexp body-sexp)
           (let ((args (map (lambda (a)
                              (match a
                                ((reg ': ty)
                                 (let ((t (deserialize-type ty)))
                                   (okm-assert-guard
                                     ((valid-register-name? reg)
                                      t)
                                     (cons reg t))))
                                (_ (begin (okm-assert #f) #f))))
                            args-sexp))
                 (rets (let ((single-type (deserialize-type rets-sexp)))
                         (if single-type
                             (list single-type)
                             (if (list? rets-sexp)
                                 (let ((types (map deserialize-type rets-sexp)))
                                   (if (for-all core-type? types)
                                       types
                                       (begin (okm-assert #f) #f)))
                                 (begin (okm-assert #f) #f)))))
                 (body (region-deserialize body-sexp)))
             (okm-assert-guard
               ((okm-valid-symbol-name? name)
                (for-all pair? args)
                rets
                body)
               (make-func name args rets body))))
          (_ #f)))))
)

