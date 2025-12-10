module Core.LibSpecs (libTests) where

import Test.Tasty (TestTree, testGroup)

--
-- public
--

libTests :: TestTree
libTests =
  testGroup
    "Lib Tests"
    []
