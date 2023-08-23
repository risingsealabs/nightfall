module Main (main) where

import Examples.Fun
import Examples.Simple
import Examples.Loops
import Examples.Inputs
import Examples.Cond

import Nightfall.MASM
import Nightfall.Targets.Miden
import Nightfall.Lang.Types

import Data.Char
import Data.String
import System.FilePath
import Control.Monad.State
import Test.Tasty
import Test.Tasty.Golden

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
            name = filter (not . isSpace) $ pName prog
            path = "test" </> "golden" </> name
        in goldenVsString name path . pure . fromString $ ppMASM masm

main :: IO ()
main = defaultMain test_examplesGolden
