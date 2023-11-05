{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Evaluation
    ( test_evaluation
    ) where

import Nightfall.Lang.Internal.Types
import Nightfall.Lang.Types
import Nightfall.MASM.Miden
import Nightfall.MASM.Types
import Nightfall.Prelude
import Nightfall.Targets.Miden

import Control.Lens
import Control.Monad.State
import Data.Word
import GHC.Natural
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.QuickCheck
import qualified Data.Map as Map

instance Arbitrary Felt where
    arbitrary = fromInteger <$> arbitrary
    shrink = map fromInteger . shrink . unFelt

instance Arbitrary MidenWord where
    arbitrary = MidenWord <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    shrink (MidenWord x0 x1 x2 x3) =
        shrink (x0, x1, x2, x3) <&> \(x0', x1', x2', x3') -> MidenWord x0' x1' x2' x3'

evalZKProgram :: ZKProgram -> IO (Either String [Felt])
evalZKProgram prog = do
    let (masm, _) = runState (transpileZKProgram prog) defaultContext
    runMiden DontKeep Nothing masm

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
        case errOrRes of
            Left err       -> fail err
            Right []       -> fail "'runMiden' returned an empty stack"
            -- The looked up element must be equal to the index of the element.
            Right (i' : _) -> pure $ fromIntegral i === i'

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
        -- greater than any of them.
        x <- pick $ (fromIntegral (maxBound :: Word8) +) . fromIntegral . succ <$> arbitrary @Word32
        errOrRes <- liftIO . evalZKProgram . mkSimpleProgram name $ do
            -- Initialize @arr@.
            initArray "arr" [0 .. fromIntegral m]
            -- Change the element at the @i@th index.
            setAt "arr" (fromIntegral i) $ lit x
            -- Get the element at the @j@th index.
            ret $ getAt "arr" $ fromIntegral j
        case errOrRes of
            Left err      -> fail err
            Right []      -> fail "'runMiden' returned an empty stack"
            Right (y : _) ->
                -- If @i@ and @j@ are equal, then we looked up the updated element and it has to be
                -- equal to @x@. Otherwise, it must be some initial element that cannot be equal to
                -- @x@.
                pure $ if i == j then x === y else fromIntegral j === y .&&. x =/= y

test_initLoadAll :: TestTree
test_initLoadAll =
    let name = "initLoadAll"
    in testProperty name $ \(xs :: [Word64]) -> withMaxSuccess 50 . monadicIO $ do
        let inputs = map fromIntegral xs
            numWords = genericLength inputs
            inputsAsWords = padFeltsAsMidenWords inputs
            hashModule = toHashModule (fromIntegral numWords) inputsAsWords
            loadAll = Program $ concat
                [ replicate 5 Drop
                , map (MemLoad . Just . unsafeToMemoryIndex) [0 .. numWords - 1]
                ]
        -- Load the inputs into memory using 'toHashModule', then drop what's on the stack, then put
        -- all the loaded inputs onto the stack one by one.
        errOrRes <- liftIO . runMiden DontKeep (Just $ fromIntegral numWords) $
            hashModule & moduleProg <>~ loadAll
        case errOrRes of
            Left err      -> fail err
            Right outputs -> pure $ inputs === reverse outputs

test_wordIsLitteEndianInMemory :: TestTree
test_wordIsLitteEndianInMemory =
    testProperty "wordIsLitteEndianInMemory" $ \(midenWord :: MidenWord) -> monadicIO $ do
        let instrs =
                [ Push $ midenWordToFelts midenWord
                , MemStorew . Just $ unsafeToMemoryIndex 0
                , MemLoad . Just $ unsafeToMemoryIndex 0
                ]
        let masm = Module [] Map.empty (Program instrs) $ Left emptySecretInputs
        errOrRes <- liftIO $ runMiden DontKeep Nothing masm
        case errOrRes of
            Left err       -> fail err
            Right []       -> fail "'runMiden' returned an empty stack"
            Right (x0 : _) -> pure $ x0 === _midenWord0 midenWord

newtype HugeNatural = HugeNatural Natural
    deriving (Show)

unHugeNatural :: HugeNatural -> Natural
unHugeNatural = coerce

instance Arbitrary HugeNatural where
    arbitrary = do
        addendums <- listOf $ do
            -- TODO: magnitudes should be chosen uniformly, not actual integers, since the latter
            -- means most are going to be greater than 128 and we want to test lesser ones too.
            powerOf2 <- chooseInteger (0, 800)
            coeff <- chooseInteger (0, 2 ^: 32 - 1)
            pure $ 2 ^ powerOf2 * coeff
        pure . HugeNatural . fromIntegral $ sum addendums

    shrink = map (HugeNatural . fromInteger) . shrink . toInteger . unHugeNatural

test_naturalToMidenWords :: TestTree
test_naturalToMidenWords =
    testProperty "naturalToMidenWords" . withMaxSuccess 10000 $ \(HugeNatural n) ->
        n === midenWordsToNatural (naturalToMidenWords n)

shrinkOne :: Arbitrary a => [a] -> [[a]]
shrinkOne []     = []
shrinkOne (x:xs) = [x':xs | x' <- shrink x] ++ [x:xs' | xs' <- shrinkOne xs]

test_setNatural :: TestTree
test_setNatural =
    let name = "setNatural"
    in testProperty name . withMaxSuccess 20 $
        forAll (chooseInt (1, 6)) $ \natsLen ->
        forAllShrink (chooseInt (0, natsLen - 1)) shrink $ \target ->
        forAllShrink (vectorOf natsLen arbitrary) shrinkOne $ \hugeNats -> do
            let nats = map unHugeNatural hugeNats
                prog = mkSimpleProgram "setNatural" $
                    for_ (zip [0 :: Int ..] nats) $ \(i, n) ->
                        statement . DeclVariable VarNat ("n" ++ show i) . unExpr $ dyn n
                (masm, _) = runState (transpileZKProgram prog) defaultContext
                offset = dynamicMemoryHead +
                    sum (map (succ . genericLength . naturalToMidenWords) $ take target nats)
                midenWords = naturalToMidenWords $ nats !! target
                propSize = monadicIO $ do
                    errOrRes <- liftIO . runMiden DontKeep (Just 1) $ masm & moduleProg <>~
                        Program [MemLoad . toMemoryIndex $ unFelt offset]
                    case errOrRes of
                        Left err      -> fail err
                        Right outputs -> pure $ [fromIntegral $ length midenWords] === outputs
                propLimbs = zip [0 ..] midenWords <&> \(limbNum, midenWord) -> monadicIO $ do
                    errOrRes <- liftIO . runMiden DontKeep (Just 4) $ masm & moduleProg <>~
                        Program [MemLoadw . toMemoryIndex . unFelt $ offset + 1 + limbNum]
                    case errOrRes of
                        Left err      -> fail err
                        Right outputs -> pure $ midenWordToFelts midenWord === reverse outputs
            conjoin $ propSize : propLimbs

test_evaluation :: TestTree
test_evaluation =
    testGroup "Evaluation"
        [ test_initGet
        , test_initSetGet
        , test_initLoadAll
        , test_wordIsLitteEndianInMemory
        , test_naturalToMidenWords
        , test_setNatural
        ]
