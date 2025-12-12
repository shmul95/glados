module IR.TestUtils
  ( emptyState,
    runGen
  )
where

import Control.Monad.State (evalState)
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

runGen :: IRGen a -> a
runGen action = evalState action emptyState
