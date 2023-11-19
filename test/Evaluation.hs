{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Evaluation
    ( test_evaluation
    , HugeNatural
    , unHugeNatural
    ) where

import Nightfall.Alphabet
import Nightfall.Lang.Internal.Types
import Nightfall.Lang.Types
import Nightfall.MASM.Miden
import Nightfall.MASM.Types
import Nightfall.Targets.Miden

import Control.Lens
import Control.Monad.State
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.QuickCheck
import qualified Data.Map as Map

-- | A list of ranges: @[(0, 10), (11, 100), (101, 1000), ... (10^n + 1, highest)]@ for @base = 10@.
magnitudesPositive :: (Num a, Integral a, Ord a) => a -> a -> [(a, a)]
magnitudesPositive base highest =
    zipWith (\low high -> (low + 1, high)) borders (tail borders)
  where
    preborders = tail . takeWhile (< highest `div` base) $ iterate (* base) 1
    borders = -1 : preborders ++ [last preborders * base, highest]

-- | Generate asymptotically greater numbers with exponentially lower chance.
arbitraryPositive :: (Bounded a, Num a, Integral a, Ord a) => a -> a -> Gen a
arbitraryPositive base highest =
    frequency . zip freqs . reverse . map chooseBoundedIntegral $ magnitudesPositive base highest
  where
    freqs = map floor $ iterate (* 1.3) (2 :: Double)

-- >>> shrinkIntegralFast (0 :: Integer)
-- []
-- >>> shrinkIntegralFast (1 :: Integer)
-- [0]
-- >>> shrinkIntegralFast (9 :: Integer)
-- [0,3,5,7,8]
-- >>> shrinkIntegralFast (-10000 :: Integer)
-- [0,-100,-5000,-7500,-8750,-9375,-9688,-9844,-9922,-9961,-9981,-9991,-9996,-9998,-9999,10000]
shrinkIntegralFast :: Integral a => a -> [a]
shrinkIntegralFast x = concat
    [ [0 | x /= 0]
    , [signum x * floor (sqrt @Double $ fromIntegral xA) | let xA = abs x, xA > 4]
    , drop 1 . map (x -) . takeWhile (/= 0) $ iterate (`quot` 2) x
    , [-x | x < 0]
    ]

-- >>> import Nightfall.Lang.Internal.Types
-- >>> import Test.Tasty.QuickCheck
-- >>> sample' (arbitrary :: Gen Felt)
-- [406240826,4,5943,214843025361102877,750349539,133,660708,99,4010657904678,154063,209048]
instance Arbitrary Felt where
    arbitrary = fromIntegral <$> arbitraryPositive 10 (maxBound @Word64)
    shrink = map fromInteger . shrinkIntegralFast . unFelt

instance Arbitrary a => Arbitrary (MidenWordOf a) where
    arbitrary = MidenWord <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    shrink (MidenWord x0 x1 x2 x3) =
        shrink (x0, x1, x2, x3) <&> \(x0', x1', x2', x3') -> MidenWord x0' x1' x2' x3'

newtype Limb = Limb
    { unLimb :: MidenWord
    }

instance Show Limb where
    show = show . unLimb

instance Arbitrary Limb where
    arbitrary = Limb . fmap (fromIntegral @Word32) <$> arbitrary
    shrink = map Limb . shrink . unLimb

evalZKProgram :: Transpile asm => Maybe Word32 -> ZKProgramAsm asm -> IO (Either String [Felt])
evalZKProgram keep prog = do
    let (masm, _) = runState (transpileZKProgram prog) defaultContext
    runMiden DontKeep keep masm

test_initGet :: TestTree
test_initGet =
    let name = "initGet"
    in testProperty name $ \(m :: Word8) -> withMaxSuccess 30 . monadicIO $ do
        -- Pick an arbitrary valid index in the array.
        i <- pick $ chooseBoundedIntegral (0, m)
        errOrRes <- liftIO . evalZKProgram (Just 1) . mkSimpleProgram name $ do
            -- Initialize @arr@.
            initArray "arr" [0 .. fromIntegral m]
            -- Get the element at the @i@th index.
            ret $ getAt "arr" $ fromIntegral i
        case errOrRes of
            Left err -> fail err
            -- The looked up element must be equal to the index of the element.
            Right [i'] -> pure $ fromIntegral i === i'
            Right _ -> fail "Wrong number of outputs"

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
        errOrRes <- liftIO . evalZKProgram (Just 1) . mkSimpleProgram name $ do
            -- Initialize @arr@.
            initArray "arr" [0 .. fromIntegral m]
            -- Change the element at the @i@th index.
            setAt "arr" (fromIntegral i) $ lit x
            -- Get the element at the @j@th index.
            ret $ getAt "arr" $ fromIntegral j
        case errOrRes of
            Left err -> fail err
            Right [y] ->
                -- If @i@ and @j@ are equal, then we looked up the updated element and it has to be
                -- equal to @x@. Otherwise, it must be some initial element that cannot be equal to
                -- @x@.
                pure $ if i == j then x === y else fromIntegral j === y .&&. x =/= y
            Right _ -> fail "Wrong number of outputs"

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
            powerOf2 <- toInteger <$> arbitraryPositive 4 (1000 :: Word32)
            coeff <- toInteger <$> arbitraryPositive 10 (maxBound :: Word32)
            pure $ 2 ^ powerOf2 * coeff
        pure . HugeNatural . fromIntegral $ sum addendums

    -- By default at each shrink QuickCheck at most divides the number by two, which makes the
    -- number smaller way too slow. We add square root to speed up the process.
    shrink = coerce . shrinkIntegralFast . unHugeNatural

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
                prog = mkSimpleProgram name $
                    for_ (zip [0 :: Int ..] nats) $ \(i, n) ->
                        statement . DeclVariable VarNat ("n" ++ show i) . unExpr $ lit n
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

test_addLimbs :: TestTree
test_addLimbs =
    let name = "addLimbs"
    in testProperty name . withMaxSuccess 110 $ \(Limb mword1) (Limb mword2) ->
        monadicIO $ do
            let prog = mkSimpleProgramAsm name $
                    ret $ binOp Add256
                        (assembly [Push $ midenWordToFelts mword1, Padw])
                        (assembly [Push $ midenWordToFelts mword2, Padw])
            errOrRes <- liftIO $ evalZKProgram (Just 8) prog
            case errOrRes of
                Left err      -> fail err
                Right outputs -> do
                    let expected = midenWordsToNatural [mword1] + midenWordsToNatural [mword2]
                    pure $ expected === feltsToNatural (reverse outputs)

-- TODO: test length too.
test_addNats :: TestTree
test_addNats =
    let name = "addNats"
    in testProperty name . withMaxSuccess 70 $ \(HugeNatural nat1) (HugeNatural nat2) ->
        monadicIO $ do
            let prog = mkSimpleProgramAsm name $ do
                    ret $ addNat (lit nat1) (lit nat2)
                    loadNat
                expected = nat1 + nat2
                numFelts = fromIntegral $ length (naturalToMidenWords expected) * 4
            errOrRes <- liftIO $ evalZKProgram (Just numFelts) prog
            case errOrRes of
                Left err      -> error err
                Right outputs -> pure $ expected === feltsToNatural (reverse outputs)

test_evaluation :: TestTree
test_evaluation =
    testGroup "Evaluation"
        [ test_initGet
        , test_initSetGet
        , test_initLoadAll
        , test_wordIsLitteEndianInMemory
        , test_naturalToMidenWords
        , test_setNatural
        , test_addLimbs
        , test_addNats
        ]
