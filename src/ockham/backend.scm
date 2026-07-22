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
             (okm-assert t)
             (if t (make-constant t value) #f)))
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
             (okm-assert t)
             (okm-assert (valid-register-name? operand))
             (if (and t (valid-register-name? operand))
                 (make-copy t operand)
                 #f)))
          (_ #f)))))

  (define-dialect-op (be add)
    (fields
      (immutable type add-type)
      (immutable lhs add-lhs)
      (immutable rhs add-rhs))
    (serializer
      (lambda (op)
        (okm-assert (or (int? (add-type op)) (ptr? (add-type op))))
        (okm-assert (valid-register-name? (add-lhs op)))
        (okm-assert (valid-register-name? (add-rhs op)))
        `(_ ,(add-lhs op) ,(add-rhs op) : ,(serialize-type (add-type op)))))
    (deserializer
      (lambda (lst)
        (match lst
          ((_ lhs rhs ': ty)
           (let ((t (deserialize-type ty)))
             (okm-assert (and t (or (int? t) (ptr? t))))
             (okm-assert (valid-register-name? lhs))
             (okm-assert (valid-register-name? rhs))
             (if (and t (or (int? t) (ptr? t))
                      (valid-register-name? lhs)
                      (valid-register-name? rhs))
                 (make-add t lhs rhs)
                 #f)))
          (_ #f)))))

  (define-dialect-op (be sub)
    (fields
      (immutable type sub-type)
      (immutable lhs sub-lhs)
      (immutable rhs sub-rhs))
    (serializer
      (lambda (op)
        (okm-assert (or (int? (sub-type op)) (ptr? (sub-type op))))
        (okm-assert (valid-register-name? (sub-lhs op)))
        (okm-assert (valid-register-name? (sub-rhs op)))
        `(_ ,(sub-lhs op) ,(sub-rhs op) : ,(serialize-type (sub-type op)))))
    (deserializer
      (lambda (lst)
        (match lst
          ((_ lhs rhs ': ty)
           (let ((t (deserialize-type ty)))
             (okm-assert (and t (or (int? t) (ptr? t))))
             (okm-assert (valid-register-name? lhs))
             (okm-assert (valid-register-name? rhs))
             (if (and t (or (int? t) (ptr? t))
                      (valid-register-name? lhs)
                      (valid-register-name? rhs))
                 (make-sub t lhs rhs)
                 #f)))
          (_ #f)))))

  (define-dialect-op (be mul)
    (fields
      (immutable type mul-type)
      (immutable lhs mul-lhs)
      (immutable rhs mul-rhs))
    (serializer
      (lambda (op)
        (okm-assert (int? (mul-type op)))
        (okm-assert (valid-register-name? (mul-lhs op)))
        (okm-assert (valid-register-name? (mul-rhs op)))
        `(_ ,(mul-lhs op) ,(mul-rhs op) : ,(serialize-type (mul-type op)))))
    (deserializer
      (lambda (lst)
        (match lst
          ((_ lhs rhs ': ty)
           (let ((t (deserialize-type ty)))
             (okm-assert (and t (int? t)))
             (okm-assert (valid-register-name? lhs))
             (okm-assert (valid-register-name? rhs))
             (if (and t (int? t)
                      (valid-register-name? lhs)
                      (valid-register-name? rhs))
                 (make-mul t lhs rhs)
                 #f)))
          (_ #f)))))

  (define-dialect-op (be idiv)
    (fields
      (immutable type idiv-type)
      (immutable lhs idiv-lhs)
      (immutable rhs idiv-rhs))
    (serializer
      (lambda (op)
        (okm-assert (int? (idiv-type op)))
        (okm-assert (valid-register-name? (idiv-lhs op)))
        (okm-assert (valid-register-name? (idiv-rhs op)))
        `(_ ,(idiv-lhs op) ,(idiv-rhs op) : ,(serialize-type (idiv-type op)))))
    (deserializer
      (lambda (lst)
        (match lst
          ((_ lhs rhs ': ty)
           (let ((t (deserialize-type ty)))
             (okm-assert (and t (int? t)))
             (okm-assert (valid-register-name? lhs))
             (okm-assert (valid-register-name? rhs))
             (if (and t (int? t)
                      (valid-register-name? lhs)
                      (valid-register-name? rhs))
                 (make-idiv t lhs rhs)
                 #f)))
          (_ #f)))))

  (define-dialect-op (be udiv)
    (fields
      (immutable type udiv-type)
      (immutable lhs udiv-lhs)
      (immutable rhs udiv-rhs))
    (serializer
      (lambda (op)
        (okm-assert (int? (udiv-type op)))
        (okm-assert (valid-register-name? (udiv-lhs op)))
        (okm-assert (valid-register-name? (udiv-rhs op)))
        `(_ ,(udiv-lhs op) ,(udiv-rhs op) : ,(serialize-type (udiv-type op)))))
    (deserializer
      (lambda (lst)
        (match lst
          ((_ lhs rhs ': ty)
           (let ((t (deserialize-type ty)))
             (okm-assert (and t (int? t)))
             (okm-assert (valid-register-name? lhs))
             (okm-assert (valid-register-name? rhs))
             (if (and t (int? t)
                      (valid-register-name? lhs)
                      (valid-register-name? rhs))
                 (make-udiv t lhs rhs)
                 #f)))
          (_ #f)))))

  (define-dialect-op (be lshift)
    (fields
      (immutable type lshift-type)
      (immutable lhs lshift-lhs)
      (immutable rhs lshift-rhs))
    (serializer
      (lambda (op)
        (okm-assert (int? (lshift-type op)))
        (okm-assert (valid-register-name? (lshift-lhs op)))
        (okm-assert (valid-register-name? (lshift-rhs op)))
        `(_ ,(lshift-lhs op) ,(lshift-rhs op) : ,(serialize-type (lshift-type op)))))
    (deserializer
      (lambda (lst)
        (match lst
          ((_ lhs rhs ': ty)
           (let ((t (deserialize-type ty)))
             (okm-assert (and t (int? t)))
             (okm-assert (valid-register-name? lhs))
             (okm-assert (valid-register-name? rhs))
             (if (and t (int? t)
                      (valid-register-name? lhs)
                      (valid-register-name? rhs))
                 (make-lshift t lhs rhs)
                 #f)))
          (_ #f)))))

  (define-dialect-op (be rshift)
    (fields
      (immutable type rshift-type)
      (immutable lhs rshift-lhs)
      (immutable rhs rshift-rhs))
    (serializer
      (lambda (op)
        (okm-assert (int? (rshift-type op)))
        (okm-assert (valid-register-name? (rshift-lhs op)))
        (okm-assert (valid-register-name? (rshift-rhs op)))
        `(_ ,(rshift-lhs op) ,(rshift-rhs op) : ,(serialize-type (rshift-type op)))))
    (deserializer
      (lambda (lst)
        (match lst
          ((_ lhs rhs ': ty)
           (let ((t (deserialize-type ty)))
             (okm-assert (and t (int? t)))
             (okm-assert (valid-register-name? lhs))
             (okm-assert (valid-register-name? rhs))
             (if (and t (int? t)
                      (valid-register-name? lhs)
                      (valid-register-name? rhs))
                 (make-rshift t lhs rhs)
                 #f)))
          (_ #f)))))

  (define-dialect-op (be irem)
    (fields
      (immutable type irem-type)
      (immutable lhs irem-lhs)
      (immutable rhs irem-rhs))
    (serializer
      (lambda (op)
        (okm-assert (int? (irem-type op)))
        (okm-assert (valid-register-name? (irem-lhs op)))
        (okm-assert (valid-register-name? (irem-rhs op)))
        `(_ ,(irem-lhs op) ,(irem-rhs op) : ,(serialize-type (irem-type op)))))
    (deserializer
      (lambda (lst)
        (match lst
          ((_ lhs rhs ': ty)
           (let ((t (deserialize-type ty)))
             (okm-assert (and t (int? t)))
             (okm-assert (valid-register-name? lhs))
             (okm-assert (valid-register-name? rhs))
             (if (and t (int? t)
                      (valid-register-name? lhs)
                      (valid-register-name? rhs))
                 (make-irem t lhs rhs)
                 #f)))
          (_ #f)))))

  (define-dialect-op (be urem)
    (fields
      (immutable type urem-type)
      (immutable lhs urem-lhs)
      (immutable rhs urem-rhs))
    (serializer
      (lambda (op)
        (okm-assert (int? (urem-type op)))
        (okm-assert (valid-register-name? (urem-lhs op)))
        (okm-assert (valid-register-name? (urem-rhs op)))
        `(_ ,(urem-lhs op) ,(urem-rhs op) : ,(serialize-type (urem-type op)))))
    (deserializer
      (lambda (lst)
        (match lst
          ((_ lhs rhs ': ty)
           (let ((t (deserialize-type ty)))
             (okm-assert (and t (int? t)))
             (okm-assert (valid-register-name? lhs))
             (okm-assert (valid-register-name? rhs))
             (if (and t (int? t)
                      (valid-register-name? lhs)
                      (valid-register-name? rhs))
                 (make-urem t lhs rhs)
                 #f)))
          (_ #f)))))

  (define-dialect-op (be sext)
    (fields
      (immutable type sext-type)
      (immutable operand sext-operand))
    (serializer
      (lambda (op)
        (okm-assert (int? (sext-type op)))
        (okm-assert (valid-register-name? (sext-operand op)))
        `(_ ,(sext-operand op) : ,(serialize-type (sext-type op)))))
    (deserializer
      (lambda (lst)
        (match lst
          ((_ operand ': ty)
           (let ((t (deserialize-type ty)))
             (okm-assert (and t (int? t)))
             (okm-assert (valid-register-name? operand))
             (if (and t (int? t) (valid-register-name? operand))
                 (make-sext t operand)
                 #f)))
          (_ #f)))))

  (define-dialect-op (be zext)
    (fields
      (immutable type zext-type)
      (immutable operand zext-operand))
    (serializer
      (lambda (op)
        (okm-assert (int? (zext-type op)))
        (okm-assert (valid-register-name? (zext-operand op)))
        `(_ ,(zext-operand op) : ,(serialize-type (zext-type op)))))
    (deserializer
      (lambda (lst)
        (match lst
          ((_ operand ': ty)
           (let ((t (deserialize-type ty)))
             (okm-assert (and t (int? t)))
             (okm-assert (valid-register-name? operand))
             (if (and t (int? t) (valid-register-name? operand))
                 (make-zext t operand)
                 #f)))
          (_ #f)))))

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
             (okm-assert t)
             (okm-assert (valid-register-name? ptr))
             (if (and t (valid-register-name? ptr))
                 (make-load t ptr offset)
                 #f)))
          ((_ ptr ': ty)
           (let ((t (deserialize-type ty)))
             (okm-assert t)
             (okm-assert (valid-register-name? ptr))
             (if (and t (valid-register-name? ptr))
                 (make-load t ptr 0)
                 #f)))
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
             (okm-assert t)
             (okm-assert (valid-register-name? ptr))
             (okm-assert (valid-register-name? val))
             (if (and t (valid-register-name? ptr) (valid-register-name? val))
                 (make-store t ptr val offset)
                 #f)))
          ((_ ptr val ': ty)
           (let ((t (deserialize-type ty)))
             (okm-assert t)
             (okm-assert (valid-register-name? ptr))
             (okm-assert (valid-register-name? val))
             (if (and t (valid-register-name? ptr) (valid-register-name? val))
                 (make-store t ptr val 0)
                 #f)))
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
           (okm-assert (pair? target))
           (when (and (pair? target) (pair? (cdr target)))
             (for-each (lambda (arg) (okm-assert (valid-register-name? arg))) (cdr target)))
           (if (and (pair? target)
                    (or (null? (cdr target))
                        (for-all valid-register-name? (cdr target))))
               (make-jmp target)
               #f))
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
           (okm-assert (valid-register-name? condition))
           (okm-assert (pair? then-target))
           (okm-assert (pair? else-target))
           (when (and (pair? then-target) (pair? (cdr then-target)))
             (for-each (lambda (arg) (okm-assert (valid-register-name? arg))) (cdr then-target)))
           (when (and (pair? else-target) (pair? (cdr else-target)))
             (for-each (lambda (arg) (okm-assert (valid-register-name? arg))) (cdr else-target)))
           (if (and (valid-register-name? condition)
                    (pair? then-target)
                    (pair? else-target)
                    (or (null? (cdr then-target)) (for-all valid-register-name? (cdr then-target)))
                    (or (null? (cdr else-target)) (for-all valid-register-name? (cdr else-target))))
               (make-br-cond condition then-target else-target)
               #f))
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
           (okm-assert (integer? id))
           (okm-assert (and (list? args) (<= (length args) 6)))
           (when (list? args)
             (for-each (lambda (arg) (okm-assert (valid-register-name? arg))) args))
           (if (and (integer? id)
                    (list? args)
                    (<= (length args) 6)
                    (for-all valid-register-name? args))
               (make-syscall id args)
               #f))
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
           (okm-assert (okm-valid-symbol-name? name))
           (let ((args (map (lambda (a)
                              (match a
                                ((reg ': ty)
                                 (okm-assert (valid-register-name? reg))
                                 (let ((t (deserialize-type ty)))
                                   (okm-assert t)
                                   (if (and (valid-register-name? reg) t)
                                       (cons reg t)
                                       #f)))
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
             (okm-assert rets)
             (okm-assert body)
             (if (and (okm-valid-symbol-name? name)
                      (for-all pair? args)
                      rets
                      body)
                 (make-func name args rets body)
                 #f)))
          (_ #f)))))
)

