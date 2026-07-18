(be:module basic.okm
         (be:extern (@x i8))
         (be:global-int (@z i64 20))
         (be:global-bytes (@str "hello, world!\n"))
         (be:func @fib (%a : i32) -> i32
           (region
             (block ^bb0 ()
               (%cond = (int 1) (lte %a 1))
               (be:br-if %cond (^bb1) (^bb2)))
             (block ^bb1 ()
               (be:ret %a))
             (block ^bb2 ()
               (%b = (be:sub %a 1))
               (%c = (be:sub %a 2))
               (%d = (be:call @fib %b))
               (%e = (be:call @fib %c))
               (%f = (be:add %d %e))
               (be:ret %f)))))
