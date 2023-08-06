module Examples.Loops ( sumTo10Prog
                      ) where

import Nightfall.Lang.Types

-- * Simple program that adds numbers from 1 to 10

-- Haskell version
{-
sumTo10 :: Felt
sumTo10 = sumTo10' 10 0
    where sumTo10' :: Felt -> Felt -> Felt
          sumTo10' 0 acc = acc
          sumTo10' n acc = sumTo10' (n-1) (acc + n)
-}

-- DSL version
sumTo10Stmts :: Body ()
sumTo10Stmts = do
    comment "Simple programs that sums numbers from 0 to 10."
    comment "It should return 55"
    declareVarF "n" 10
    declareVarF "acc" 0
    while (varF "n" `gt` 0) $ do
        assignVarF "acc" (varF "acc" + varF "n")
        assignVarF "n" (varF "n" - 1)
    ret $ varF "acc"

sumTo10Prog :: ZKProgram
sumTo10Prog = mkSimpleProgram "Sum to 10" sumTo10Stmts
