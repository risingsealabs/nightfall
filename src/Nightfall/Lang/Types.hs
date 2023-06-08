module Nightfall.Lang.Types ( Felt
                            , VarName
                            , FunName
                            , Expr
                            , Statement
                            , ZKProgram(..)
                            , mkSimpleProgram
                            , mkZKProgram
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
                            , isOdd
                            , varF
                            , varB
                            -- , fcall
                            , nextSecretF
                            , nextSecretB
                            , declareVarF
                            , declareVarB
                            , assignVarF
                            , assignVarB
                            , ifElse
                            , simpleIf
                            , while
                            -- , nakedCall
                            , ret
                            , comment
                            , emptyLine
                            , incVarF
                            , decVarF
                            , updateVarF
                            , withVarF
                            , withVarF2
                            , withVarF3
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

-- | Helper to build a @ZKPeogram
mkZKProgram :: String -> [Statement] -> [Felt] -> FilePath -> ZKProgram
mkZKProgram name stmts pubs secretFP = ZKProgram
    { pName = name
    , pStatements = stmts
    , pPublicInputs = pubs
    , pSecretInputs = secretFP
    }

-- ** A few helpers for common code patterns

-- | Apply a function of 1 argument over @var and store result in @targetVar
withVarF :: VarName -> VarName -> (Expr Felt -> Expr Felt) -> Statement
withVarF var targetVar f = assignVarF targetVar $ f . varF $ var

-- | Apply a function of 2 arguments over two variables and store result in @targetVar
withVarF2 :: (VarName, VarName) -> VarName -> (Expr Felt -> Expr Felt -> Expr Felt) -> Statement
withVarF2 (var1, var2) targetVar f = assignVarF targetVar $ f (varF var1) (varF var2)

-- | Apply a function of 3 arguments over three variables and store result in @targetVar
withVarF3 :: (VarName, VarName, VarName) -> VarName -> (Expr Felt -> Expr Felt -> Expr Felt -> Expr Felt) -> Statement
withVarF3 (var1, var2, var3) targetVar f = assignVarF targetVar $ f (varF var1) (varF var2) (varF var3)

-- | Shorthand to update a variable value with a computation (which might depend on its current value)
-- It's shorter and easier to write 'updateVarF "cnt" $ \n -> 3 * n + 1' than writing 'assignVarF "cnt" (varF "cnt" * 3 + 1)'
-- especially if we use the variable value several times
updateVarF :: VarName -> (Expr Felt -> Expr Felt) -> Statement
-- updateVarF varname f = assignVarF varname (f (varF varname))
updateVarF varname = withVarF varname varname

-- | Shorthand to increment a variable (i += n)
-- It's shorter and easier to write 'incVarF "i" 1' than writing 'assignVarF "i" (varF "i" + 1)`
incVarF :: VarName -> Felt -> Statement
-- incVarF varname val = assignVarF varname (varF varname + (Expr . Lit $ val))
incVarF varname val = updateVarF varname (`add` lit val)

-- | Same as above, but for decrementing
decVarF :: VarName -> Felt -> Statement
decVarF varname val = updateVarF varname (`sub` lit val)