module Rune.Semantics.OpType 
  ( iHTBinary
  , sameType
  , isIntegerType
  , isFloatType
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

notAny :: Type -> Type -> Type
notAny TypeAny t = t
notAny t _ = t

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
unrank t r = Left $ printf "\n\tFailed to unrank: type family doesn't support rank %d (type: %s)" r (show t)

inferHigherType :: Type -> Type -> Either String Type
-- a is of same type as b
inferHigherType a b = unrank (notAny a b) (max (rank a) (rank b)) 

adjustType :: BinaryOp -> Type -> Type -> Either String Type
adjustType Mul a b = unrank (notAny a b) 4
adjustType Add a b = do
  higherType <- inferHigherType a b
  unrank (notAny a b) $ min 4 (rank higherType + 1)
adjustType _   a b = inferHigherType a b

iHTBinary :: BinaryOp -> Type -> Type -> Either String Type
iHTBinary op a b
  | isBoolOp op  = Right TypeBool
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

isBoolOp :: BinaryOp -> Bool
isBoolOp op = op `elem` [Eq, Neq, Lt, Lte, Gt, Gte, And, Or]

sameStruct :: Type -> Type -> Bool
sameStruct (TypeCustom a) (TypeCustom b) = a == b
sameStruct _ _ = False

sameType :: Type -> Type -> Bool
sameType a b | isInt    a && isInt    b = True
             | isUInt   a && isUInt   b = True
             | isFloat  a && isFloat  b = True
             | isChar   a && isChar   b = True
             | isString a && isString b = True
             | isBool   a && isBool   b = True
             | isNull   a && isNull   b = True
             | sameStruct a b           = True
             | otherwise                = False

isIntegerType :: Type -> Bool
isIntegerType TypeI8 = True
isIntegerType TypeI16 = True
isIntegerType TypeI32 = True
isIntegerType TypeI64 = True
isIntegerType TypeU8 = True
isIntegerType TypeU16 = True
isIntegerType TypeU32 = True
isIntegerType TypeU64 = True
isIntegerType _ = False

isFloatType :: Type -> Bool
isFloatType TypeF32 = True
isFloatType TypeF64 = True
isFloatType _ = False
