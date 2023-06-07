-- module Examples.Fib () where

-- import Nightfall.Lang.Types
-- import Data.Word ( Word64 )

{-
-- Original program, written in imperative style for example.
-- Compute the 33'th Fibonnaci number
fib :: Word64-> Word64
fib 0 = 1
fib 1 = 1
fib n i j = let newN = n-1
                newI = j
                newJ = i + j
            in fib newN newI newJ
-}

{-
-- Original program, written in imperative style for example.
-- Compute the n-th Fibonnaci number starting with the two provided numbers
fib :: Word64 -> Word64 -> Word64 -> Word64
fib 0 _ j = j
fib n i j = let newN = n-1
                newI = j
                newJ = i + j
            in fib newN newI newJ
-}

-- fibStmts :: [Statement]
-- fibStmt = 