(library (ockham backend)
  (export)
  (import (rnrs)
          (ufo-match)
          (ockham core))

  ; TODO: Impelement core operators
  (define backend-operators
    '(const copy add sub mul idiv udiv lshift rshift irem urem
      load store jmp br-cond syscall call ret func global-int global-bytes module extern))
)
