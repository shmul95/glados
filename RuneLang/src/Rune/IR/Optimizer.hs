{-# OPTIONS_GHC -cpp #-}

#if defined(TESTING_EXPORT)
module Rune.IR.Optimizer
  ( runIROptimizer
  )
where
#else
module Rune.IR.Optimizer
  ( runIROptimizer
  )
where
#endif

import Rune.IR.Nodes (IRProgram)

--
-- public
--

runIROptimizer :: IRProgram -> IRProgram
runIROptimizer p = p
