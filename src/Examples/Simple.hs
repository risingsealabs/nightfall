module Examples.Simple ( trivial1Prog
                       , trivial2Prog
                       , trivial3Prog
                       , trivial4Prog
                       , simpleVar1Prog
                       , simpleVar2Prog
                       , simpleVar3Prog
                       ) where

import Nightfall.Lang.Types

-- * Simplest, most trivial program that adds to fixed numbers, no variable, no inputs (public or secret)

-- | That's the Haskell program to describe with the EDSL
{-
trivial1 :: Felt
trivial1 = 33 + 42
-}

-- | That's the EDSL version
trivial1Body :: Body ()
trivial1Body = do
    comment "Simply add fixed numbers 33 + 42, should output 75"
    ret $ add (lit 33) (lit 42)

trivial1Prog :: ZKProgram
trivial1Prog = ZKProgram { pName = "trivial 1"
                          , pBody = trivial1Body
                          , pPublicInputs = []
                          , pSecretInputs = Left []
                          }

-- * Almost as simple a program, we just have one nested level

-- | That's the Haskell program to describe with the EDSL
{-
trivial2 :: Felt
trivial2 = 29 + 156 + 14 
-}

-- | That's the EDSL version
trivial2Body :: Body ()
trivial2Body = do
    comment "Simply add three numbers, 29 + 156 + 14, should output 199"
    ret $ add (lit 29) (add (lit 156) (lit 14))

trivial2Prog :: ZKProgram
trivial2Prog = ZKProgram { pName = "trivial 2"
                          , pBody = trivial2Body
                          , pPublicInputs = []
                          , pSecretInputs = Left []
                          }


-- * This time we have a multiplication and an addition, we need to make sure orders of evaluation is correct

-- | That's the Haskell program to describe with the EDSL
{-
trivial3 :: Felt
trivial3 = 1238 * (345 + 78)
-}

-- | That's the EDSL version
trivial3Body :: Body ()
trivial3Body = do
    comment "Perform an addition followed by a multipication, 1238 * (345 + 78), should output 523674"
    ret $ mul (lit 1238) (add (lit 345) (lit 78))

trivial3Prog :: ZKProgram
trivial3Prog = ZKProgram { pName = "trivial 3"
                          , pBody = trivial3Body
                          , pPublicInputs = []
                          , pSecretInputs = Left []
                          }

-- * Another simple program that uses the Num instance t omake it easier to write and make sure it works

-- | That's the Haskell program to describe with the EDSL
{-
trivial4 :: Felt
trivial4 = 52 * (11 - 1)
-}

-- | That's the EDSL version
trivial4Body :: Body ()
trivial4Body = do
    comment "Performs 52 * (11 - 1), written with Num instance, should output 520"
    ret $ 52 * (11 - 1)

trivial4Prog :: ZKProgram
trivial4Prog = ZKProgram { pName = "trivial 4"
                          , pBody = trivial4Body
                          , pPublicInputs = []
                          , pSecretInputs = Left []
                          }

-- * Simple program using a variable to store a value

-- Haskell program
{-
simpleVar1 :: Felt
simpleVar1 = let a = 999
             in a + 1
-}


-- EDSL version
simpleVar1Body :: Body ()
simpleVar1Body = do
    comment "Simple addition, but with a variable storing a value"
    comment "a = 999"
    comment "a + 1. It should return 1000"
    declareVarF "a" (lit 999)
    ret $ add (varF "a") (lit 1)

simpleVar1Prog :: ZKProgram
simpleVar1Prog = ZKProgram { pName = "simple var 1"
                          , pBody = simpleVar1Body
                          , pPublicInputs = []
                          , pSecretInputs = Left []
                          }


-- * Same program as before, but uses more variables and make a subtraction. Makes sure values are fetched correctly

-- Haskell program
{-
simpleVar2 :: Felt
simpleVar2 = let a = 888
                 b = 222
                 diff = a - b
             in diff
-}


-- EDSL version
simpleVar2Body :: Body ()
simpleVar2Body = do
    comment "Simple subtraction, but uses three variables"
    comment "a = 888, b = 222"
    comment "c = a - b. Return c"
    comment "It should return 666"
    declareVarF "a" 888
    declareVarF "b" 222
    declareVarF "c" (varF "a" - varF "b")
    ret $ varF "c"

simpleVar2Prog :: ZKProgram
simpleVar2Prog = ZKProgram { pName = "simple var 2"
                          , pBody = simpleVar2Body
                          , pPublicInputs = []
                          , pSecretInputs = Left []
                          }

-- * Simple program that overwrite the values of a variable several times

simpleVar3Body :: Body ()
simpleVar3Body = do
    comment "Rewrite on the same variable several times"
    comment "a = 10, b = 20, a = 50, a + b. Should return 70"
    declareVarF "a" 10
    declareVarF "b" 20
    assignVarF "a" 50
    ret $ varF "a" + varF "b"

simpleVar3Prog :: ZKProgram
simpleVar3Prog = mkSimpleProgram "simple var 3" simpleVar3Body
