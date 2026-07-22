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
          store-serialize store-deserialize)
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
        `(_ ,(constant-value op) : ,(int-serialize (constant-type op)))))
    (deserializer
      (lambda (lst)
        (match lst
          ((_ value ': ty) (make-constant (int-deserialize ty) value))
          (_ #f)))))

  (define-dialect-op (be copy)
    (fields
      (immutable type copy-type)
      (immutable operand copy-operand))
    (serializer
      (lambda (op)
        `(_ ,(copy-operand op) : ,(int-serialize (copy-type op)))))
    (deserializer
      (lambda (lst)
        (match lst
          ((_ operand ': ty) (make-copy (int-deserialize ty) operand))
          (_ #f)))))

  (define-dialect-op (be add)
    (fields
      (immutable type add-type)
      (immutable lhs add-lhs)
      (immutable rhs add-rhs))
    (serializer
      (lambda (op)
        `(_ ,(add-lhs op) ,(add-rhs op) : ,(int-serialize (add-type op)))))
    (deserializer
      (lambda (lst)
        (match lst
          ((_ lhs rhs ': ty) (make-add (int-deserialize ty) lhs rhs))
          (_ #f)))))

  (define-dialect-op (be sub)
    (fields
      (immutable type sub-type)
      (immutable lhs sub-lhs)
      (immutable rhs sub-rhs))
    (serializer
      (lambda (op)
        `(_ ,(sub-lhs op) ,(sub-rhs op) : ,(int-serialize (sub-type op)))))
    (deserializer
      (lambda (lst)
        (match lst
          ((_ lhs rhs ': ty) (make-sub (int-deserialize ty) lhs rhs))
          (_ #f)))))

  (define-dialect-op (be mul)
    (fields
      (immutable type mul-type)
      (immutable lhs mul-lhs)
      (immutable rhs mul-rhs))
    (serializer
      (lambda (op)
        `(_ ,(mul-lhs op) ,(mul-rhs op) : ,(int-serialize (mul-type op)))))
    (deserializer
      (lambda (lst)
        (match lst
          ((_ lhs rhs ': ty) (make-mul (int-deserialize ty) lhs rhs))
          (_ #f)))))

  (define-dialect-op (be idiv)
    (fields
      (immutable type idiv-type)
      (immutable lhs idiv-lhs)
      (immutable rhs idiv-rhs))
    (serializer
      (lambda (op)
        `(_ ,(idiv-lhs op) ,(idiv-rhs op) : ,(int-serialize (idiv-type op)))))
    (deserializer
      (lambda (lst)
        (match lst
          ((_ lhs rhs ': ty) (make-idiv (int-deserialize ty) lhs rhs))
          (_ #f)))))

  (define-dialect-op (be udiv)
    (fields
      (immutable type udiv-type)
      (immutable lhs udiv-lhs)
      (immutable rhs udiv-rhs))
    (serializer
      (lambda (op)
        `(_ ,(udiv-lhs op) ,(udiv-rhs op) : ,(int-serialize (udiv-type op)))))
    (deserializer
      (lambda (lst)
        (match lst
          ((_ lhs rhs ': ty) (make-udiv (int-deserialize ty) lhs rhs))
          (_ #f)))))

  (define-dialect-op (be lshift)
    (fields
      (immutable type lshift-type)
      (immutable lhs lshift-lhs)
      (immutable rhs lshift-rhs))
    (serializer
      (lambda (op)
        `(_ ,(lshift-lhs op) ,(lshift-rhs op) : ,(int-serialize (lshift-type op)))))
    (deserializer
      (lambda (lst)
        (match lst
          ((_ lhs rhs ': ty) (make-lshift (int-deserialize ty) lhs rhs))
          (_ #f)))))

  (define-dialect-op (be rshift)
    (fields
      (immutable type rshift-type)
      (immutable lhs rshift-lhs)
      (immutable rhs rshift-rhs))
    (serializer
      (lambda (op)
        `(_ ,(rshift-lhs op) ,(rshift-rhs op) : ,(int-serialize (rshift-type op)))))
    (deserializer
      (lambda (lst)
        (match lst
          ((_ lhs rhs ': ty) (make-rshift (int-deserialize ty) lhs rhs))
          (_ #f)))))

  (define-dialect-op (be irem)
    (fields
      (immutable type irem-type)
      (immutable lhs irem-lhs)
      (immutable rhs irem-rhs))
    (serializer
      (lambda (op)
        `(_ ,(irem-lhs op) ,(irem-rhs op) : ,(int-serialize (irem-type op)))))
    (deserializer
      (lambda (lst)
        (match lst
          ((_ lhs rhs ': ty) (make-irem (int-deserialize ty) lhs rhs))
          (_ #f)))))

  (define-dialect-op (be urem)
    (fields
      (immutable type urem-type)
      (immutable lhs urem-lhs)
      (immutable rhs urem-rhs))
    (serializer
      (lambda (op)
        `(_ ,(urem-lhs op) ,(urem-rhs op) : ,(int-serialize (urem-type op)))))
    (deserializer
      (lambda (lst)
        (match lst
          ((_ lhs rhs ': ty) (make-urem (int-deserialize ty) lhs rhs))
          (_ #f)))))

  (define-dialect-op (be sext)
    (fields
      (immutable type sext-type)
      (immutable operand sext-operand))
    (serializer
      (lambda (op)
        `(_ ,(sext-operand op) : ,(int-serialize (sext-type op)))))
    (deserializer
      (lambda (lst)
        (match lst
          ((_ operand ': ty) (make-sext (int-deserialize ty) operand))
          (_ #f)))))

  (define-dialect-op (be zext)
    (fields
      (immutable type zext-type)
      (immutable operand zext-operand))
    (serializer
      (lambda (op)
        `(_ ,(zext-operand op) : ,(int-serialize (zext-type op)))))
    (deserializer
      (lambda (lst)
        (match lst
          ((_ operand ': ty) (make-zext (int-deserialize ty) operand))
          (_ #f)))))

  (define-dialect-op (be load)
    (fields
      (immutable type load-type)
      (immutable ptr load-ptr)
      (immutable offset load-offset))
    (serializer
      (lambda (op)
        (if (and (load-offset op) (not (zero? (load-offset op))))
            `(_ ,(load-ptr op) ,(load-offset op) : ,(int-serialize (load-type op)))
            `(_ ,(load-ptr op) : ,(int-serialize (load-type op))))))
    (deserializer
      (lambda (lst)
        (match lst
          ((_ ptr offset ': ty) (make-load (int-deserialize ty) ptr offset))
          ((_ ptr ': ty) (make-load (int-deserialize ty) ptr 0))
          (_ #f)))))

  (define-dialect-op (be store)
    (fields
      (immutable type store-type)
      (immutable ptr store-ptr)
      (immutable val store-val)
      (immutable offset store-offset))
    (serializer
      (lambda (op)
        (if (and (store-offset op) (not (zero? (store-offset op))))
            `(_ ,(store-ptr op) ,(store-val op) ,(store-offset op) : ,(int-serialize (store-type op)))
            `(_ ,(store-ptr op) ,(store-val op) : ,(int-serialize (store-type op))))))
    (deserializer
      (lambda (lst)
        (match lst
          ((_ ptr val offset ': ty) (make-store (int-deserialize ty) ptr val offset))
          ((_ ptr val ': ty) (make-store (int-deserialize ty) ptr val 0))
          (_ #f)))))
)

