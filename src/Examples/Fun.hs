module Examples.Fun ( collatzFixedProg
                    ) where

import Nightfall.Lang.Types


-- * The Collatz Sequence, start from 10

-- Haskell version
{-
collatzFixed :: Felt
collatzFixed = collatz' 10
    where collataz' :: Felt -> Felt
          collatz 1 = 1
          collatz n | odd n = collatz (3 * n + 1)
                    | otherwise = collatz (n `div` 2)
-}

-- | DSL version
collatzFixedStmts :: [Statement]
collatzFixedStmts = [ comment "Compute the Collatz sequence, starting from a fixed position: 10"
               , comment "It should return 1"
               , emptyLine
               , declareVarF "start" 10
               , declareVarF "n" (varF "start")
               , while (varF "n" `gt` 1) [ifElse (isOdd (varF "n"))
                                            [assignVarF "n" (varF "n" * 3 + 1)]
                                            [assignVarF "n" (varF "n" `div'` 2)]
                                         ]
               , ret . Just $ varF "n"
               ]

collatzFixedProg :: ZKProgram
collatzFixedProg = mkSimpleProgram "Fixed Collatz (10)" collatzFixedStmts