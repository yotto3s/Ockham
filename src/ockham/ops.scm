(library (ockham ops)
  (export constant make-constant constant?
          constant-type constant-value
          constant-serialize constant-deserialize

          copy make-copy copy?
          copy-operand
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

          sdiv make-sdiv sdiv?
          sdiv-type sdiv-lhs sdiv-rhs
          sdiv-serialize sdiv-deserialize

          udiv make-udiv udiv?
          udiv-type udiv-lhs udiv-rhs
          udiv-serialize udiv-deserialize

          lshift make-lshift lshift?
          lshift-type lshift-lhs lshift-rhs
          lshift-serialize lshift-deserialize

          rshift make-rshift rshift?
          rshift-type rshift-lhs rshift-rhs
          rshift-serialize rshift-deserialize

          srem make-srem srem?
          srem-type srem-lhs srem-rhs
          srem-serialize srem-deserialize

          urem make-urem urem?
          urem-type urem-lhs urem-rhs
          urem-serialize urem-deserialize

          cmpeq make-cmpeq cmpeq?
          cmpeq-type cmpeq-lhs cmpeq-rhs
          cmpeq-serialize cmpeq-deserialize

          cmpne make-cmpne cmpne?
          cmpne-type cmpne-lhs cmpne-rhs
          cmpne-serialize cmpne-deserialize

          slt make-slt slt?
          slt-type slt-lhs slt-rhs
          slt-serialize slt-deserialize

          ult make-ult ult?
          ult-type ult-lhs ult-rhs
          ult-serialize ult-deserialize

          sgt make-sgt sgt?
          sgt-type sgt-lhs sgt-rhs
          sgt-serialize sgt-deserialize

          ugt make-ugt ugt?
          ugt-type ugt-lhs ugt-rhs
          ugt-serialize ugt-deserialize

          sle make-sle sle?
          sle-type sle-lhs sle-rhs
          sle-serialize sle-deserialize

          ule make-ule ule?
          ule-type ule-lhs ule-rhs
          ule-serialize ule-deserialize

          sge make-sge sge?
          sge-type sge-lhs sge-rhs
          sge-serialize sge-deserialize

          uge make-uge uge?
          uge-type uge-lhs uge-rhs
          uge-serialize uge-deserialize

          sext make-sext sext?
          sext-operand
          sext-serialize sext-deserialize

          zext make-zext zext?
          zext-operand
          zext-serialize zext-deserialize

          load make-load load?
          load-ptr load-offset
          load-serialize load-deserialize

          store make-store store?
          store-ptr store-val store-offset
          store-serialize store-deserialize

          br make-br br?
          br-target br-args
          br-serialize br-deserialize

          br-cond make-br-cond br-cond?
          br-cond-condition br-cond-then-target br-cond-then-args br-cond-else-target br-cond-else-args
          br-cond-serialize br-cond-deserialize

          syscall make-syscall syscall?
          syscall-id syscall-args
          syscall-serialize syscall-deserialize

          call make-call call?
          call-callee call-args
          call-serialize call-deserialize

          ret make-ret ret?
          ret-args
          ret-serialize ret-deserialize

          func make-func func?
          func-name func-args func-return-types func-body
          func-serialize func-deserialize

          extern make-extern extern?
          extern-name extern-type
          extern-serialize extern-deserialize

          module make-module module?
          module-name module-body
          module-serialize module-deserialize

          init-ops!)
  (import (rnrs (6))
          (ufo-match)
          (ockham core))

  (define-syntax define-op
    (lambda (stx)
      (syntax-case stx ()
        ((_ op-name (fields field-spec ...) (serializer ser-body) (deserializer deser-body))
         (let* ((op-sym (syntax->datum #'op-name))
                (str (symbol->string op-sym))
                (make-sym (string->symbol (string-append "make-" str)))
                (pred-sym (string->symbol (string-append str "?")))
                (ser-sym (string->symbol (string-append str "-serialize")))
                (deser-sym (string->symbol (string-append str "-deserialize"))))
           (with-syntax ((make-proc (datum->syntax #'op-name make-sym))
                         (pred-proc (datum->syntax #'op-name pred-sym))
                         (ser-proc (datum->syntax #'op-name ser-sym))
                         (deser-proc (datum->syntax #'op-name deser-sym)))
             #'(begin
                 (define-record-type (op-name make-proc pred-proc)
                   (fields field-spec ...))
                 (define ser-proc ser-body)
                 (define deser-proc deser-body))))))))

  (define-record-type (constant %make-constant constant?)
    (fields
      (immutable type constant-type)
      (immutable value constant-value)))

  (define make-constant
    (case-lambda
      ((type value) (%make-constant type value))
      ((value) (%make-constant #f value))))

  (define (constant-serialize op)
    (if (constant-type op)
        `(constant ,(serialize-type (constant-type op)) ,(constant-value op))
        `(constant ,(constant-value op))))

  (define (constant-deserialize lst)
    (okm-match lst
      (('constant ty-sexp value)
       (let ((t (deserialize-type ty-sexp)))
         (if t
             (make-constant t value)
             (make-constant ty-sexp))))
      (('constant value) (make-constant value))))

  (define-op copy
    (fields
      (immutable operand copy-operand))
    (serializer
      (lambda (op)
        (okm-assert (valid-register-name? (copy-operand op)))
        `(copy ,(copy-operand op))))
    (deserializer
      (lambda (lst)
        (okm-match lst
          (('copy operand)
           (okm-assert-guard
             ((valid-register-name? operand))
             (make-copy operand)))))))

  (define-syntax define-binary-op
    (lambda (stx)
      (syntax-case stx ()
        ((_ name)
         (let* ((name-sym (syntax->datum #'name))
                (str (symbol->string name-sym))
                (make-sym (string->symbol (string-append "make-" str)))
                (raw-make-sym (string->symbol (string-append "%make-" str)))
                (type-sym (string->symbol (string-append str "-type")))
                (lhs-sym (string->symbol (string-append str "-lhs")))
                (rhs-sym (string->symbol (string-append str "-rhs")))
                (pred-sym (string->symbol (string-append str "?")))
                (ser-sym (string->symbol (string-append str "-serialize")))
                (deser-sym (string->symbol (string-append str "-deserialize"))))
           (with-syntax ((make-op (datum->syntax #'name make-sym))
                         (raw-make (datum->syntax #'name raw-make-sym))
                         (op-type (datum->syntax #'name type-sym))
                         (op-lhs (datum->syntax #'name lhs-sym))
                         (op-rhs (datum->syntax #'name rhs-sym))
                         (pred-op (datum->syntax #'name pred-sym))
                         (ser-op (datum->syntax #'name ser-sym))
                         (deser-op (datum->syntax #'name deser-sym)))
             #'(begin
                 (define-record-type (name raw-make pred-op)
                   (fields
                     (immutable type op-type)
                     (immutable lhs op-lhs)
                     (immutable rhs op-rhs)))
                 (define make-op
                   (case-lambda
                     ((type lhs rhs) (raw-make type lhs rhs))
                     ((lhs rhs) (raw-make #f lhs rhs))))
                 (define ser-op
                   (lambda (op)
                     (okm-assert (valid-register-name? (op-lhs op)))
                     (okm-assert (valid-register-name? (op-rhs op)))
                     (if (op-type op)
                         `(name ,(serialize-type (op-type op)) ,(op-lhs op) ,(op-rhs op))
                         `(name ,(op-lhs op) ,(op-rhs op)))))
                 (define deser-op
                   (lambda (lst)
                     (okm-match lst
                       (('name ty-sexp lhs rhs)
                        (let ((t (deserialize-type ty-sexp)))
                          (okm-assert-guard
                            (t
                             (valid-register-name? lhs)
                             (valid-register-name? rhs))
                            (make-op t lhs rhs))))
                       (('name lhs rhs)
                        (okm-assert-guard
                          ((valid-register-name? lhs)
                           (valid-register-name? rhs))
                          (make-op lhs rhs)))))))))))))

  (define-syntax define-binary-ops
    (syntax-rules ()
      ((_ op ...)
       (begin (define-binary-op op) ...))))

  (define-syntax define-extension-op
    (lambda (stx)
      (syntax-case stx ()
        ((_ name)
         (let* ((name-sym (syntax->datum #'name))
                (str (symbol->string name-sym))
                (make-sym (string->symbol (string-append "make-" str)))
                (operand-sym (string->symbol (string-append str "-operand"))))
           (with-syntax ((make-op (datum->syntax #'name make-sym))
                         (op-operand (datum->syntax #'name operand-sym)))
             #'(define-op name
                 (fields
                   (immutable operand op-operand))
                 (serializer
                   (lambda (op)
                     (okm-assert (valid-register-name? (op-operand op)))
                     `(name ,(op-operand op))))
                 (deserializer
                   (lambda (lst)
                     (okm-match lst
                       (('name operand)
                        (okm-assert-guard
                          ((valid-register-name? operand))
                          (make-op operand)))))))))))))

  (define-binary-ops add sub mul sdiv udiv lshift rshift srem urem cmpeq cmpne slt ult sgt ugt sle ule sge uge)

  (define-extension-op sext)
  (define-extension-op zext)

  (define-op load
    (fields
      (immutable ptr load-ptr)
      (immutable offset load-offset))
    (serializer
      (lambda (op)
        (okm-assert (valid-register-name? (load-ptr op)))
        (if (and (load-offset op) (not (zero? (load-offset op))))
            `(load ,(load-ptr op) ,(load-offset op))
            `(load ,(load-ptr op)))))
    (deserializer
      (lambda (lst)
        (okm-match lst
          (('load ptr offset)
           (okm-assert-guard
             ((valid-register-name? ptr))
             (make-load ptr offset)))
          (('load ptr)
           (okm-assert-guard
             ((valid-register-name? ptr))
             (make-load ptr 0)))))))

  (define-op store
    (fields
      (immutable ptr store-ptr)
      (immutable val store-val)
      (immutable offset store-offset))
    (serializer
      (lambda (op)
        (okm-assert (valid-register-name? (store-ptr op)))
        (okm-assert (valid-register-name? (store-val op)))
        (if (and (store-offset op) (not (zero? (store-offset op))))
            `(store ,(store-ptr op) ,(store-val op) ,(store-offset op))
            `(store ,(store-ptr op) ,(store-val op)))))
    (deserializer
      (lambda (lst)
        (okm-match lst
          (('store ptr val offset)
           (okm-assert-guard
             ((valid-register-name? ptr)
              (valid-register-name? val))
             (make-store ptr val offset)))
          (('store ptr val)
           (okm-assert-guard
             ((valid-register-name? ptr)
              (valid-register-name? val))
             (make-store ptr val 0)))))))

  (define (parse-br-target-spec sexp)
    (if (pair? sexp)
        (values (car sexp) (cdr sexp))
        (values sexp '())))

  (define-record-type (br %make-br br?)
    (fields
      (immutable target br-target)
      (immutable args br-args)))

  (define (br-serialize op)
    (let ((tgt (br-target op))
          (args (br-args op)))
      (okm-assert (symbol? tgt))
      (okm-assert (list? args))
      (for-each (lambda (arg) (okm-assert (valid-register-name? arg))) args)
      (if (null? args)
          `(br ,tgt)
          `(br (,tgt . ,args)))))

  (define (br-deserialize lst)
    (okm-match lst
      (('br spec)
       (let-values (((tgt args) (parse-br-target-spec spec)))
         (okm-assert-guard
           ((symbol? tgt)
            (list? args)
            (for-all valid-register-name? args))
           (%make-br tgt args))))))

  (define make-br
    (case-lambda
      ((target) (%make-br target '()))
      ((target args) (%make-br target args))))

  (define-record-type (br-cond %make-br-cond br-cond?)
    (fields
      (immutable condition br-cond-condition)
      (immutable then-target br-cond-then-target)
      (immutable then-args br-cond-then-args)
      (immutable else-target br-cond-else-target)
      (immutable else-args br-cond-else-args)))

  (define (br-cond-serialize op)
    (okm-assert (valid-register-name? (br-cond-condition op)))
    (let ((then-t (br-cond-then-target op))
          (then-a (br-cond-then-args op))
          (else-t (br-cond-else-target op))
          (else-a (br-cond-else-args op)))
      (okm-assert (symbol? then-t))
      (okm-assert (list? then-a))
      (for-each (lambda (arg) (okm-assert (valid-register-name? arg))) then-a)
      (okm-assert (symbol? else-t))
      (okm-assert (list? else-a))
      (for-each (lambda (arg) (okm-assert (valid-register-name? arg))) else-a)
      (let ((then-part (if (null? then-a) then-t `(,then-t . ,then-a)))
            (else-part (if (null? else-a) else-t `(,else-t . ,else-a))))
        `(br-cond ,(br-cond-condition op) ,then-part ,else-part))))

  (define (br-cond-deserialize lst)
    (okm-match lst
      (('br-cond condition then-spec else-spec)
       (let-values (((then-t then-a) (parse-br-target-spec then-spec))
                    ((else-t else-a) (parse-br-target-spec else-spec)))
         (okm-assert-guard
           ((valid-register-name? condition)
            (symbol? then-t)
            (list? then-a)
            (for-all valid-register-name? then-a)
            (symbol? else-t)
            (list? else-a)
            (for-all valid-register-name? else-a))
           (%make-br-cond condition then-t then-a else-t else-a))))))

  (define make-br-cond
    (case-lambda
      ((condition then-target else-target)
       (%make-br-cond condition then-target '() else-target '()))
      ((condition then-target then-args else-target else-args)
       (%make-br-cond condition then-target then-args else-target else-args))))

  (define-op syscall
    (fields
      (immutable id syscall-id)
      (immutable args syscall-args))
    (serializer
      (lambda (op)
        (okm-assert (integer? (syscall-id op)))
        (let ((args (syscall-args op)))
          (okm-assert (and (list? args) (<= (length args) 6)))
          (for-each (lambda (arg) (okm-assert (valid-register-name? arg))) args)
          `(syscall ,(syscall-id op) . ,args))))
    (deserializer
      (lambda (lst)
        (okm-match lst
          (('syscall id . args)
           (okm-assert-guard
             ((integer? id)
              (list? args)
              (<= (length args) 6)
              (for-all valid-register-name? args))
             (make-syscall id args)))))))

  (define-op call
    (fields
      (immutable callee call-callee)
      (immutable args call-args))
    (serializer
      (lambda (op)
        (let ((callee (call-callee op))
              (args (call-args op)))
          (okm-assert (or (valid-register-name? callee) (okm-valid-symbol-name? callee)))
          (okm-assert (and (list? args) (for-all valid-register-name? args)))
          `(call ,callee . ,args))))
    (deserializer
      (lambda (lst)
        (okm-match lst
          (('call callee . args)
           (okm-assert-guard
             ((or (valid-register-name? callee) (okm-valid-symbol-name? callee))
              (list? args)
              (for-all valid-register-name? args))
             (make-call callee args)))))))

  (define-record-type (ret %make-ret ret?)
    (fields
      (immutable args ret-args)))

  (define (ret-serialize op)
    (let ((args (ret-args op)))
      (okm-assert (and (list? args) (for-all valid-register-name? args)))
      `(ret . ,args)))

  (define (ret-deserialize lst)
    (okm-match lst
      (('ret . args)
       (okm-assert-guard
         ((list? args)
          (for-all valid-register-name? args))
         (%make-ret args)))))

  (define make-ret
    (case-lambda
      (() (%make-ret '()))
      ((args) (%make-ret args))))

  (define (func-build-sexp op)
    (okm-assert (okm-valid-symbol-name? (func-name op)))
    (let ((args-sexp (map (lambda (a)
                            (okm-assert (valid-register-name? (car a)))
                            (list (car a) (serialize-type (cdr a))))
                          (func-args op)))
          (rets-sexp (map serialize-type (func-return-types op)))
          (body-sexp (region-serialize (func-body op))))
      (list 'func (func-name op) args-sexp '-> rets-sexp body-sexp)))

  (define-op func
    (fields
      (immutable name func-name)
      (immutable args func-args)
      (immutable return-types func-return-types)
      (immutable body func-body))
    (serializer func-build-sexp)
    (deserializer
      (lambda (lst)
        (okm-match lst
          (('func name args-sexp '-> rets-sexp body-sexp)
           (let ((args (map (lambda (a)
                              (okm-match a
                                ((reg ty)
                                 (let ((t (deserialize-type ty)))
                                   (okm-assert-guard
                                     ((valid-register-name? reg)
                                      t)
                                     (cons reg t))))))
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
               (make-func name args rets body))))))))

  (define-op extern
    (fields
      (immutable name extern-name)
      (immutable type extern-type))
    (serializer
      (lambda (op)
        (okm-assert (okm-valid-symbol-name? (extern-name op)))
        (okm-assert (core-type? (extern-type op)))
        `(extern ,(extern-name op) ,(serialize-type (extern-type op)))))
    (deserializer
      (lambda (lst)
        (okm-match lst
          (('extern name ty-sexp)
           (let ((t (deserialize-type ty-sexp)))
             (okm-assert-guard
               ((okm-valid-symbol-name? name)
                t)
               (make-extern name t))))))))

  (define-op module
    (fields
      (immutable name module-name)
      (immutable body module-body))
    (serializer
      (lambda (op)
        (okm-assert (symbol? (module-name op)))
        (okm-assert (region? (module-body op)))
        `(module ,(module-name op) ,(region-serialize (module-body op)))))
    (deserializer
      (lambda (lst)
        (okm-match lst
          (('module name body-sexp)
           (let ((body (region-deserialize body-sexp)))
             (okm-assert-guard
               ((symbol? name)
                body)
               (make-module name body))))))))

  (define (init-ops!)
    (register-op 'constant constant-serialize constant-deserialize)
    (register-op 'copy copy-serialize copy-deserialize)
    (register-op 'add add-serialize add-deserialize)
    (register-op 'sub sub-serialize sub-deserialize)
    (register-op 'mul mul-serialize mul-deserialize)
    (register-op 'sdiv sdiv-serialize sdiv-deserialize)
    (register-op 'udiv udiv-serialize udiv-deserialize)
    (register-op 'lshift lshift-serialize lshift-deserialize)
    (register-op 'rshift rshift-serialize rshift-deserialize)
    (register-op 'srem srem-serialize srem-deserialize)
    (register-op 'urem urem-serialize urem-deserialize)
    (register-op 'cmpeq cmpeq-serialize cmpeq-deserialize)
    (register-op 'cmpne cmpne-serialize cmpne-deserialize)
    (register-op 'slt slt-serialize slt-deserialize)
    (register-op 'ult ult-serialize ult-deserialize)
    (register-op 'sgt sgt-serialize sgt-deserialize)
    (register-op 'ugt ugt-serialize ugt-deserialize)
    (register-op 'sle sle-serialize sle-deserialize)
    (register-op 'ule ule-serialize ule-deserialize)
    (register-op 'sge sge-serialize sge-deserialize)
    (register-op 'uge uge-serialize uge-deserialize)
    (register-op 'sext sext-serialize sext-deserialize)
    (register-op 'zext zext-serialize zext-deserialize)
    (register-op 'load load-serialize load-deserialize)
    (register-op 'store store-serialize store-deserialize)
    (register-op 'br br-serialize br-deserialize)
    (register-op 'br-cond br-cond-serialize br-cond-deserialize)
    (register-op 'syscall syscall-serialize syscall-deserialize)
    (register-op 'call call-serialize call-deserialize)
    (register-op 'ret ret-serialize ret-deserialize)
    (register-op 'func func-serialize func-deserialize)
    (register-op 'extern extern-serialize extern-deserialize)
    (register-op 'module module-serialize module-deserialize))

  (init-ops!)
)
