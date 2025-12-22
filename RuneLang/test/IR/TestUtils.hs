module IR.TestUtils
  ( emptyState,
    runGen,
    runGenUnsafe
  )
where

import Control.Monad.State (evalState)
import Control.Monad.Except (runExceptT)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.HashMap.Strict as HM
import Rune.IR.Nodes

--
-- public
--

emptyState :: GenState
emptyState = GenState
  { gsTempCounter = 0,
    gsLabelCounter = 0,
    gsStringCounter = 0,
    gsFloatCounter = 0,
    gsGlobals = [],
    gsCurrentFunc = Nothing,
    gsSymTable = Map.empty,
    gsStructs = Map.empty,
    gsLoopStack = [],
    gsCalledFuncs = Set.empty,
    gsStringMap = Map.empty,
    gsFloatMap = Map.empty,
    gsFuncStack = HM.empty
  }

runGen :: IRGen a -> Either String a
runGen action = evalState (runExceptT action) emptyState

runGenUnsafe :: IRGen a -> a
runGenUnsafe action = case runGen action of
  Right val -> val
  Left err -> error $ "IR Generation failed: " ++ err
