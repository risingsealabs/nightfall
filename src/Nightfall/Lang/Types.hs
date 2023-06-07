module Nightfall.Lang.Types ( Felt
                            , VarName
                            , FunName
                            , Expr
                            , Statement
                            , ZKProgram(..)
                            , mkSimpleProgram
                            , lit
                            , bool
                            , add
                            , sub
                            , mul
                            , div'
                            , mod'
                            , eq
                            , not'
                            , lt
                            , lte
                            , gt
                            , gte
                            , varF
                            , varB
                            -- , fcall
                            , declareVarF
                            , declareVarB
                            , assignVarF
                            , assignVarB
                            , ifElse
                            , simpleIf
                            -- , nakedCall
                            , ret
                            , comment
                            , emptyLine
                            ) where

import Nightfall.Lang.Internal.Types

-- | Helper to quickly make a simple @ZKProgram from a list of statements, no inputs
mkSimpleProgram :: String -> [Statement] -> ZKProgram
mkSimpleProgram name stmts = ZKProgram
    { pName = name
    , pStatements = stmts
    , pPublicInputs = []
    , pSecretInputs = ""
    }