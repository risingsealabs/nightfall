{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Evaluation
    ( test_evaluation
    ) where

import Nightfall.Lang.Types
import Nightfall.MASM.Miden
import Nightfall.Targets.Miden

import Control.Monad.State
import Data.Word
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.QuickCheck

evalZKProgram :: ZKProgram -> IO (Either String [Felt])
evalZKProgram prog = do
    let (masm, _) = runState (transpile prog) defaultContext
    runMiden DontKeep masm

test_initGet :: TestTree
test_initGet =
    let name = "initGet"
    in testProperty name $ \(m :: Word8) -> withMaxSuccess 30 . monadicIO $ do
        -- Pick an arbitrary valid index in the array.
        i <- pick $ chooseBoundedIntegral (0, m)
        errOrRes <- liftIO . evalZKProgram . mkSimpleProgram name $ do
            -- Initialize @arr@.
            initArray "arr" [0 .. fromIntegral m]
            -- Get the element at the @i@th index.
            ret $ getAt "arr" $ fromIntegral i
        i' <- case errOrRes of
            Left err       -> fail err
            Right []       -> fail "'runMiden' returned an empty stack"
            Right (i' : _) -> pure i'
        -- The looked up element must be equal to the index of the element.
        pure $ fromIntegral i === i'

test_initSetGet :: TestTree
test_initSetGet =
    let name = "initSetGet"
    in testProperty name $ \(m :: Word8) -> withMaxSuccess 30 . monadicIO $ do
        -- Pick an arbitrary valid index in the array.
        i <- pick $ chooseBoundedIntegral (0, m)
        -- Pick another arbitrary valid index such that it's equal to @i@ in about 25% of cases.
        j <- pick $ frequency
            [ (1, pure i)
            , (3, chooseBoundedIntegral (0, m))
            ]
        -- Pick @x@ such that it's definitely not equal to any of the elements in the array by being
        -- larger than any of them.
        x <- pick $ (fromIntegral (maxBound :: Word8) +) . fromIntegral . succ <$> arbitrary @Word32
        errOrRes <- liftIO . evalZKProgram . mkSimpleProgram name $ do
            -- Initialize @arr@.
            initArray "arr" [0 .. fromIntegral m]
            -- Change the element at the @i@th index.
            setAt "arr" (fromIntegral i) $ lit x
            -- Get the element at the @j@th index.
            ret $ getAt "arr" $ fromIntegral j
        y <- case errOrRes of
            Left err      -> fail err
            Right []      -> fail "'runMiden' returned an empty stack"
            Right (y : _) -> pure y
        -- If @i@ and @j@ are equal, then we looked up the updated element and it has to be equal to
        -- @x@. Otherwise, it must be some initial element that cannot be equal to @x@.
        pure $ if i == j then x === y else fromIntegral j === y .&&. x =/= y

test_evaluation :: TestTree
test_evaluation =
    testGroup "Evaluation"
        [ test_initGet
        , test_initSetGet
        ]
