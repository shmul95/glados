{-# LANGUAGE CPP #-}

#if defined(TESTING_EXPORT)
module Rune.Backend.Helpers
  ( emit,
    escapeString,
    collectIRVars,
    collectTopLevels,
    calculateStackMap,
    collectTopLevel,
    collectVars,
    accumulateOffset,
    encodeCharacter,
    encodeUtf8Bytes,
    makeRbpOffset,
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
import Rune.Backend.Types (Extern, Function, Global, Struct)
import Rune.IR.IRHelpers (sizeOfIRType)
import Rune.IR.Nodes (IRFunction (..), IRInstruction (..), IRTopLevel (..), IRType (..))
import Lib (isPrintable, alignTo, alignSize)
import Data.Char (ord)

type StructMap = Map.Map String Struct

--
-- public
--

emit :: Int -> String -> String
emit lvl s = replicate (lvl * 4) ' ' ++ s

collectTopLevels :: [IRTopLevel] -> ([Extern], [Global], [Function], StructMap)
collectTopLevels tls =
  let (es, gs, fs, ss) = foldr collectTopLevel ([], [], [], Map.empty) tls
   in (nub es, reverse gs, reverse fs, ss)

calculateStackMap :: StructMap -> Function -> (Map.Map String Int, Int)
calculateStackMap structMap func =
  let varsMap = collectIRVars func
      varsList = Map.toList varsMap
      (totalUsedSize, offsetsMap) = foldl' (accumulateOffset structMap) (0, Map.empty) varsList
      totalSize = alignTo 16 totalUsedSize
      rbpOffsetsMap = Map.map (makeRbpOffset totalUsedSize) offsetsMap
   in (rbpOffsetsMap, totalSize)

escapeString :: String -> String
escapeString = intercalate "," . encodeCharacter

--
-- private
--

collectTopLevel :: IRTopLevel -> ([Extern], [Global], [Function], StructMap) -> ([Extern], [Global], [Function], StructMap)
collectTopLevel (IRExtern name) (e, g, f, s) = (name : e, g, f, s)
collectTopLevel (IRGlobalDef n v) (e, g, f, s) = (e, (n, v) : g, f, s)
collectTopLevel (IRFunctionDef fn) (e, g, f, s) = (e, g, fn : f, s)
collectTopLevel (IRStructDef name fields) (e, g, f, s) = (e, g, f, Map.insert name fields s)

collectIRVars :: Function -> Map.Map String IRType
collectIRVars (IRFunction _ params _ body _) = 
  let initialMap = Map.fromList params
   in foldl' collectVars initialMap body

collectVars :: Map.Map String IRType -> IRInstruction -> Map.Map String IRType
collectVars acc (IRASSIGN n _ t) = Map.insert n t acc
collectVars acc (IRALLOC n t) = Map.insert n t acc
collectVars acc (IRLOAD n _ t) = Map.insert n t acc
collectVars acc (IRDEREF n _ t) = Map.insert n t acc
collectVars acc (IRLOAD_OFFSET n _ _ t) = Map.insert n t acc
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
collectVars acc (IRBNOT_OP n _ t) = Map.insert n t acc
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
collectVars acc (IRCAST n _ _ t) = Map.insert n t acc
collectVars acc (IRINC _) = acc
collectVars acc (IRDEC _) = acc
collectVars acc _ = acc

accumulateOffset :: StructMap -> (Int, Map.Map String Int) -> (String, IRType) -> (Int, Map.Map String Int)
accumulateOffset structMap (currentOffset, accMap) (name, irType) =
  let size = sizeOfIRType structMap irType
      align = alignSize size
      alignedOffset = alignTo align currentOffset
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
  | otherwise = encodeUtf8Bytes (ord c) ++ encodeCharacter cs

-- | Encode a Unicode code point as UTF-8 bytes (as string representations)
encodeUtf8Bytes :: Int -> [String]
encodeUtf8Bytes cp
  | cp < 0x80 = [show cp]
  | cp < 0x800 = 
      [ show (0xC0 + (cp `div` 64))
      , show (0x80 + (cp `mod` 64))
      ]
  | cp < 0x10000 =
      [ show (0xE0 + (cp `div` 4096))
      , show (0x80 + ((cp `div` 64) `mod` 64))
      , show (0x80 + (cp `mod` 64))
      ]
  | otherwise =
      [ show (0xF0 + (cp `div` 262144))
      , show (0x80 + ((cp `div` 4096) `mod` 64))
      , show (0x80 + ((cp `div` 64) `mod` 64))
      , show (0x80 + (cp `mod` 64))
      ]

-- | convert offset from function stack frame to RBP-relative offset
makeRbpOffset :: Int -> Int -> Int
makeRbpOffset totalUsedSize offset = -(totalUsedSize - offset)
