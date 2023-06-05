module Examples.Cond ( simpleIfProg
                     , ifVarProg
                     , simpleInfProg
                     ) where

import Nightfall.Lang.Types
import Nightfall.Lang.Internal.Types ( ZKProgram (..) ) -- Yeah I know, I should not. I should have a mkProgram or smth (TODO)

-- * Simple program that uses one if / else statement

-- | Haskell program
{-
simpleIf :: Felt
simpleIf = if 4 == 8
           then 10
           else 20
-}

-- | EDSL version
simpleIfStmts :: [Statement]
simpleIfStmts = [ comment "Simple, stupid and trivial program that makes uses of a condition"
                , comment "if (4 == 8) then return 10 else return 20"
                , comment "Should return 20"
                , ifElse (eq 4 8) [ret . Just $ 10] [ret . Just $ 20]
                ]

simpleIfProg :: ZKProgram
simpleIfProg = ZKProgram { pName = "simple if"
                          , pStatements = simpleIfStmts
                          , pPublicInputs = []
                          , pSecretInputs = []
                          }

-- * Simple program that uses one if / else statement on a moderately complex computation involving variables

-- | Haskell program
{-
simpleIf :: Felt
simpleIf = let a = 145
               b = 79
               target = 203
               sum = a + b
               okVal = 10
               nokVal = 20
            in if sum == target
               then okVal
               else nokVal
-}

-- | EDSL version
ifVarStmts :: [Statement]
ifVarStmts = [ comment "Makes a if/else comparison on a moderately complex computation, involving variables"
             , comment "It sums a=145 + b=79 and compares equality with target=203."
             , comment "If equal, it returns okVal=10, otherwise nokVal=20"
             , comment "It should return 20"
             , declareVar "a" 145
             , declareVar "b" 79
             , declareVar "target" 203
             , declareVar "sum" (var "a" + var "b")
             , declareVar "okVal" 10
             , declareVar "nokVal" 20
             , ifElse (var "sum" `eq` var "target") [ret . Just $ var "okVal"] [ret . Just $ var "nokVal"]
            ]

ifVarProg :: ZKProgram
ifVarProg = ZKProgram { pName = "If with vars"
                          , pStatements = ifVarStmts
                          , pPublicInputs = []
                          , pSecretInputs = []
                          }

-- * Simple program that compares two fixed numbers stored in variables and return the lowest

{-
simpleInf :: Felt
simpleInf = let n1 = 4238
                n2 = 21987
            in if n1 <= n2
               then n1
               else n2
-}

-- | EDSL version
simpleInfStmts :: [Statement]
simpleInfStmts = [ comment "if n1=4238 <= n2=21987 then n1 else n2."
                 , comment "It should return 4238"
                 , emptyLine
                 , declareVar "n1" 4238
                 , declareVar "n2" 21987
                 , ifElse (var "n1" `lte` var "n2") [ret . Just $ var "n1"] [ret . Just $ var "n2"]
                 ]

simpleInfProg :: ZKProgram
simpleInfProg = ZKProgram { pName = "simple inf"
                          , pStatements = simpleInfStmts
                          , pPublicInputs = []
                          , pSecretInputs = []
                          }