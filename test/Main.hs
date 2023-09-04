{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Examples.Fun
import Examples.Simple
import Examples.Loops
import Examples.Inputs
import Examples.Cond

import Nightfall.Lang.Types
import Nightfall.MASM
import Nightfall.MASM.Types
import Nightfall.MASM.Miden
import Nightfall.Targets.Miden

import Data.Char
import Data.String
import System.FilePath
import Control.Monad.State
import Test.Tasty
import Test.Tasty.Golden
import System.IO.Unsafe
import Data.Aeson (eitherDecodeFileStrict, (.:))
import Data.Aeson.Types (parseEither)

replaceSecretInputsFileWithContents :: Module -> Module
replaceSecretInputsFileWithContents modul = modul
    { moduleSecretInputs = do
        path <- moduleSecretInputs modul
        let adviceStack = either error id $ do
                obj <- unsafePerformIO $ eitherDecodeFileStrict path
                parseEither (.: "advice_stack") obj
        Left $ map read adviceStack
    }

test_examplesGolden :: TestTree
test_examplesGolden =
    testGroup "golden examples"
        [ goldenExample trivial1Prog
        , goldenExample trivial2Prog
        , goldenExample trivial3Prog
        , goldenExample trivial4Prog
        , goldenExample simpleVar1Prog
        , goldenExample simpleVar2Prog
        , goldenExample simpleVar3Prog
        , goldenExample sumTo10Prog
        , goldenExample simpleSecretProg
        , goldenExample collatzFixedProg
        , goldenExample collatzPrivProg
        , goldenExample simpleIfProg
        , goldenExample ifVarProg
        , goldenExample simpleInfProg
        ]
  where
    goldenExample prog =
        let context = defaultContext
                { config = defaultConfig
                    { cgfTraceVariablesDecl = True
                    , cfgTraceVariablesUsage = True
                    }
                }
            (masm, _) = runState (transpile prog) context
            name = filter isAlphaNum $ pName prog
            path = "test" </> "golden" </> name
        in testGroup name $
            [ goldenVsString "program" (path ++ ".masm") $
                pure . fromString $
                    ppMASM masm
            , goldenVsString "result" (path ++ ".mres") $
                fromString . either id show <$>
                    runMiden DontKeep masm
            ] ++
            -- Just to test that 'runMiden' correctly handles secret inputs provided via a list of
            -- 'Felt's directly, not just via an inputs file.
            [ goldenVsString "result via list" (path ++ ".mres") $
                fromString . either id show <$>
                    runMiden DontKeep (replaceSecretInputsFileWithContents masm)
            | Right _ <- [moduleSecretInputs masm]
            ]

main :: IO ()
main = defaultMain test_examplesGolden
