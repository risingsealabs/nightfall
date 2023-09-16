{-# LANGUAGE GADTs #-}

module Nightfall.Lang.Syntax.Default where

import Nightfall.Lang.Types
import Nightfall.Lang.Internal.Types

data DeclType a where
    Felt :: DeclType Felt
    Bool :: DeclType Bool

data Var a = Var
    { _varDeclType :: DeclType a
    , _varName     :: VarName
    }

class KnownDeclType a where
    knownDeclType :: DeclType a

instance KnownDeclType Felt where
    knownDeclType = Felt

instance KnownDeclType Bool where
    knownDeclType = Bool

declareOf :: DeclType a -> VarName -> Expr a -> Body (Var a)
declareOf declType varName expr = do
    statement . DeclVariable varName $ unExpr expr
    pure $ Var declType varName

declare :: KnownDeclType a => VarName -> Expr a -> Body (Var a)
declare = declareOf knownDeclType

get :: Var a -> Expr a
get (Var declType name) = case declType of
    Felt -> Expr $ VarF name
    Bool -> Expr $ VarB name

set :: Var a -> Expr a -> Body ()
set var expr = statement $ AssignVar (_varName var) (unExpr expr)

mut :: Var a -> (Expr a -> Expr a) -> Body ()
mut var f = set var . f $ get var
