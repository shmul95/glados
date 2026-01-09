{-# LANGUAGE CPP #-}

#if defined(TESTING_EXPORT)
module Rune.Backend.Helpers
  ( emit,
    escapeString,
    collectIRVars,
    collectTopLevels,
    calculateStackMap,
    alignUp,
    collectTopLevel,
    collectVars,
    accumulateOffset,
    encodeCharacter,
    makeRbpOffset
  )
where
#else
module Rune.Backend.Helpers
  ( emit,
    escapeString,
    collectIRVars,
    collectTopLevels,
    calculateStackMap,
  )
where
#endif

import Data.List (intercalate, nub)
import qualified Data.Map.Strict as Map
import Rune.Backend.Types (Extern, Function, Global)
import Rune.IR.IRHelpers (sizeOfIRType)
import Rune.IR.Nodes (IRFunction (..), IRInstruction (..), IRTopLevel (..), IRType (..))
import Lib (isPrintable)
import Data.Char (ord)

--
-- public
--

emit :: Int -> String -> String
emit lvl s = replicate (lvl * 4) ' ' ++ s

collectTopLevels :: [IRTopLevel] -> ([Extern], [Global], [Function])
collectTopLevels tls =
  let (es, gs, fs) = foldr collectTopLevel ([], [], []) tls
   in (nub es, reverse gs, reverse fs)

calculateStackMap :: Function -> (Map.Map String Int, Int)
calculateStackMap func =
  let varsMap = collectIRVars func
      varsList = Map.toList varsMap
      (totalUsedSize, offsetsMap) = foldl' (accumulateOffset varsMap) (0, Map.empty) varsList
      totalSize = alignUp totalUsedSize 16
      rbpOffsetsMap = Map.map (makeRbpOffset totalUsedSize) offsetsMap
   in (rbpOffsetsMap, totalSize)

escapeString :: String -> String
escapeString = intercalate "," . encodeCharacter

--
-- private
--

alignUp :: Int -> Int -> Int
alignUp x n = (x + n - 1) `div` n * n

collectTopLevel :: IRTopLevel -> ([Extern], [Global], [Function]) -> ([Extern], [Global], [Function])
collectTopLevel (IRExtern name) (e, g, f) = (name : e, g, f)
collectTopLevel (IRGlobalDef n v) (e, g, f) = (e, (n, v) : g, f)
collectTopLevel (IRFunctionDef fn) (e, g, f) = (e, g, fn : f)
collectTopLevel _ acc = acc

collectIRVars :: Function -> Map.Map String IRType
collectIRVars (IRFunction _ params _ body _) = 
  let initialMap = Map.fromList params
   in foldl' collectVars initialMap body

collectVars :: Map.Map String IRType -> IRInstruction -> Map.Map String IRType
collectVars acc (IRASSIGN n _ t) = Map.insert n t acc
collectVars acc (IRALLOC n t) = Map.insert n t acc
collectVars acc (IRLOAD n _ t) = Map.insert n t acc
collectVars acc (IRDEREF n _ t) = Map.insert n t acc
collectVars acc (IRGET_FIELD n _ _ _ t) = Map.insert n t acc
collectVars acc (IRGET_ELEM n _ _ t) = Map.insert n t acc

-- | collect vars for array stack allocation
-- -> length elements + 1 (for the null terminator)
-- -> array are ptr of their data
collectVars acc (IRALLOC_ARRAY n t elems) =
  let arrType = IRArray t (length elems + 1)
      ptrType = IRPtr arrType
   in Map.insert (n <> "_data") arrType (Map.insert n ptrType acc)

collectVars acc (IRADD_OP n _ _ t) = Map.insert n t acc
collectVars acc (IRSUB_OP n _ _ t) = Map.insert n t acc
collectVars acc (IRMUL_OP n _ _ t) = Map.insert n t acc
collectVars acc (IRDIV_OP n _ _ t) = Map.insert n t acc
collectVars acc (IRMOD_OP n _ _ t) = Map.insert n t acc
collectVars acc (IRSHR_OP n _ _ t) = Map.insert n t acc
collectVars acc (IRSHL_OP n _ _ t) = Map.insert n t acc
collectVars acc (IRBAND_OP n _ _ t) = Map.insert n t acc
collectVars acc (IRCMP_EQ n _ _) = Map.insert n IRBool acc
collectVars acc (IRCMP_NEQ n _ _) = Map.insert n IRBool acc
collectVars acc (IRCMP_LT n _ _) = Map.insert n IRBool acc
collectVars acc (IRCMP_LTE n _ _) = Map.insert n IRBool acc
collectVars acc (IRCMP_GT n _ _) = Map.insert n IRBool acc
collectVars acc (IRCMP_GTE n _ _) = Map.insert n IRBool acc
collectVars acc (IRAND_OP n _ _ t) = Map.insert n t acc
collectVars acc (IROR_OP n _ _ t) = Map.insert n t acc
collectVars acc (IRCALL n _ _ (Just t)) = Map.insert n t acc
collectVars acc (IRCALL n _ _ Nothing) | not (null n) = Map.insert n IRI64 acc
collectVars acc (IRADDR n _ t) = Map.insert n t acc
collectVars acc (IRINC _) = acc
collectVars acc (IRDEC _) = acc
collectVars acc _ = acc

accumulateOffset :: Map.Map String IRType -> (Int, Map.Map String Int) -> (String, IRType) -> (Int, Map.Map String Int)
accumulateOffset _ (currentOffset, accMap) (name, irType) =
  let size = sizeOfIRType irType
      align = min 8 (if size == 0 then 1 else size)
      alignedOffset = alignUp currentOffset align
      newOffset = alignedOffset + size
   in (newOffset, Map.insert name alignedOffset accMap)

encodeCharacter :: String -> [String]
encodeCharacter "" = []
encodeCharacter (c : cs)
  | c == '\n' = "10" : encodeCharacter cs
  | c == '\r' = "13" : encodeCharacter cs
  | c == '\t' = "9" : encodeCharacter cs
  | c == '\0' = "0" : encodeCharacter cs
  | isPrintable c =
      let (printables, rest) = span isPrintable (c : cs)
       in ("\"" ++ printables ++ "\"") : encodeCharacter rest
  | otherwise = show (ord c) : encodeCharacter cs

-- | convert offset from function stack frame to RBP-relative offset
makeRbpOffset :: Int -> Int -> Int
makeRbpOffset totalUsedSize offset = -(totalUsedSize - offset)
