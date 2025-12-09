module Rune.Semantics.OpType 
  ( iHTBinary
  , sameType
  ) where

import Rune.AST.Nodes (Type(..), BinaryOp(..))
import Text.Printf (printf)

--
-- compare type
--

rank :: Type -> Int
rank TypeI64 = 4
rank TypeU64 = 4
rank TypeF64 = 4
rank TypeI32 = 3
rank TypeU32 = 3
rank TypeF32 = 3
rank TypeI16 = 2
rank TypeU16 = 2
rank TypeI8  = 1
rank TypeU8  = 1
rank _       = 0

unrank :: Type -> Int -> Either String Type
unrank t 1 | isInt   t = Right TypeI8
           | isUInt  t = Right TypeU8
unrank t 2 | isInt   t = Right TypeI16
           | isUInt  t = Right TypeU16
unrank t 3 | isInt   t = Right TypeI32
           | isUInt  t = Right TypeU32
           | isFloat t = Right TypeF32
unrank t 4 | isInt   t = Right TypeI64
           | isUInt  t = Right TypeU64
           | isFloat t = Right TypeF64
unrank t 5 | isInt   t = Right TypeI64
           | isUInt  t = Right TypeU64
           | isFloat t = Right TypeF64
unrank _ _ = Left "\nsomething went wrong in unrank"

inferHigherType :: Type -> Type -> Either String Type
-- a is of same type as b
inferHigherType a b = unrank a (max (rank a) (rank b)) 

adjustType :: BinaryOp -> Type -> Type -> Either String Type
adjustType Mul a _ = unrank a 4
adjustType Add a b = do
  higherType <- inferHigherType a b
  unrank a $ rank higherType + 1
adjustType _   a b = inferHigherType a b

iHTBinary :: BinaryOp -> Type -> Type -> Either String Type
iHTBinary op a b
  | sameType a b = adjustType op a b
  | otherwise    = Left $ printf
    "WrongType: v1 has type %s and v2 has type %s, they don't match"
    (show a) (show b)

--
-- is function
--

isInt :: Type -> Bool
isInt     t = t `elem` [TypeI8, TypeI16, TypeI32, TypeI64, TypeAny]

isUInt :: Type -> Bool
isUInt    t = t `elem` [TypeU8, TypeU16, TypeU32, TypeU64, TypeAny]

isFloat :: Type -> Bool
isFloat   t = t `elem` [TypeF32, TypeF64, TypeAny]

isChar :: Type -> Bool
isChar    t = t `elem` [TypeChar  , TypeAny]

isString :: Type -> Bool
isString  t = t `elem` [TypeString, TypeAny]

isBool :: Type -> Bool
isBool    t = t == TypeBool

isNull :: Type -> Bool
isNull    t = t == TypeNull

sameType :: Type -> Type -> Bool
sameType a b | isInt    a && isInt    b = True
             | isUInt   a && isUInt   b = True
             | isFloat  a && isFloat  b = True
             | isChar   a && isChar   b = True
             | isString a && isString b = True
             | isBool   a && isBool   b = True
             | isNull   a && isNull   b = True
             | otherwise                = False

