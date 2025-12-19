module TestHelpers (dummyPos) where

import Rune.AST.Nodes (SourcePos(..))

-- | Dummy position for testing purposes
dummyPos :: SourcePos
dummyPos = SourcePos "test.ru" 0 0
