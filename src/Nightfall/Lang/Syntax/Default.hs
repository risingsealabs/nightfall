{-# LANGUAGE GADTs #-}

module Nightfall.Lang.Syntax.Default where

import Nightfall.Lang.Types
import Nightfall.Lang.Internal.Types

import GHC.Natural

data DeclType a where
    Felt :: DeclType Felt
    Bool :: DeclType Bool
    Nat  :: DeclType Natural

data Binding a = Binding
    { _varDeclType :: DeclType a
    , _varName     :: VarName
    }

class KnownDeclType a where
    knownDeclType :: DeclType a

instance KnownDeclType Felt where
    knownDeclType = Felt

instance KnownDeclType Bool where
    knownDeclType = Bool

instance KnownDeclType Natural where
    knownDeclType = Nat

toVarType :: DeclType a -> VarType
toVarType Felt = VarFelt
toVarType Bool = VarBool
toVarType Nat  = VarNat

declareOf :: DeclType a -> VarName -> Expr a -> Body (Binding a)
declareOf declType varName expr = do
    statement . DeclVariable (toVarType declType) varName $ unExpr expr
    pure $ Binding declType varName

declare :: KnownDeclType a => VarName -> Expr a -> Body (Binding a)
declare = declareOf knownDeclType

get :: Binding a -> Expr a
get (Binding _ name) = Expr $ Var name

set :: Binding a -> Expr a -> Body ()
set var expr = statement $ AssignVar (_varName var) (unExpr expr)

mut :: Binding a -> (Expr a -> Expr a) -> Body ()
mut var f = set var . f $ get var
