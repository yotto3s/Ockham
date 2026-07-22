(library (ockham backend)
  (export constant make-constant constant?
          constant-type constant-value
          constant-serialize constant-deserialize)
  (import (rnrs)
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
)
