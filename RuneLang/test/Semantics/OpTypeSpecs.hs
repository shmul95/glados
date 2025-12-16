module Semantics.OpTypeSpecs (opTypeSemanticsTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertFailure)
import Rune.Semantics.OpType
import Rune.AST.Nodes (Type(..), BinaryOp(..))

--
-- public
--

opTypeSemanticsTests :: TestTree
opTypeSemanticsTests =
  testGroup
    "Rune.Semantics.OpType"
    [ testGroup "sameType function" 
        [ testCase "returns True for same int family types" $ do
            sameType TypeI8  TypeI16 @?= True
            sameType TypeI32 TypeI64 @?= True
            sameType TypeI16 TypeAny @?= True,
        
          testCase "returns True for same uint family types" $ do
            sameType TypeU8  TypeU16 @?= True
            sameType TypeU32 TypeU64 @?= True
            sameType TypeU16 TypeAny @?= True,
        
          testCase "returns True for same float family types" $ do
            sameType TypeF32 TypeF64 @?= True
            sameType TypeF32 TypeAny @?= True,
        
          testCase "returns True for identical scalar types" $ do
            sameType TypeBool   TypeBool   @?= True
            sameType TypeChar   TypeChar   @?= True
            sameType TypeString TypeString @?= True
            sameType TypeNull   TypeNull   @?= True,

          testCase "returns True for identical custom types" $ do
            sameType (TypeCustom "Vec2f") (TypeCustom "Vec2f") @?= True,

          testCase "returns False for different custom types or custom vs primitive" $ do
            sameType (TypeCustom "A") (TypeCustom "B") @?= False
            sameType (TypeCustom "A") TypeI32 @?= False,
        
          testCase "returns False for different type families" $ do
            sameType TypeI32 TypeU32 @?= False
            sameType TypeI32 TypeF32 @?= False
            sameType TypeU32 TypeF32 @?= False
            sameType TypeI32 TypeBool @?= False
            sameType TypeF32 TypeChar @?= False
        ],

      testGroup "iHTBinary function" 
        [ testCase "handles compatible int types with Add operation" $
            iHTBinary Add TypeI32 TypeI64 @?= Right TypeI64,
        
          testCase "handles compatible uint types with Add operation" $
            iHTBinary Add TypeU32 TypeU64 @?= Right TypeU64,
        
          testCase "handles compatible float types with Add operation" $
            iHTBinary Add TypeF32 TypeF64 @?= Right TypeF64,

          testCase "handles logical operations (And, Or)" $ do
            iHTBinary And TypeBool TypeBool @?= Right TypeBool
            iHTBinary Or TypeBool TypeBool @?= Right TypeBool,

          testCase "handles comparison operations (Eq, Gt) promoting to Bool" $ do
            iHTBinary Eq TypeI32 TypeI32 @?= Right TypeBool
            iHTBinary Gt TypeF64 TypeF64 @?= Right TypeBool,
        
          testCase "handles Mul operation promoting to highest precision" $ do
            iHTBinary Mul TypeI32 TypeI32 @?= Right TypeI64
            iHTBinary Mul TypeU16 TypeU32 @?= Right TypeU64
            iHTBinary Mul TypeF32 TypeF32 @?= Right TypeF64,
        
          testCase "handles other binary operations with type inference" $ do
            iHTBinary Sub TypeI16 TypeI32 @?= Right TypeI32
            iHTBinary Div TypeU8  TypeU16 @?= Right TypeU16
            iHTBinary Mod TypeF32 TypeF64 @?= Right TypeF64,
        
          testCase "rejects operations between different type families" $
            case iHTBinary Add TypeI32 TypeU32 of
              Left _ -> return ()
              Right _  -> assertFailure "Should have failed for incompatible types",
        
          testCase "rejects int and float mixing" $
            case iHTBinary Add TypeI32 TypeF32 of
              Left _ -> return ()
              Right _  -> assertFailure "Should have failed for int/float mixing",
        
          testCase "rejects uint and float mixing" $
            case iHTBinary Add TypeU32 TypeF32 of
              Left _ -> return ()
              Right _  -> assertFailure "Should have failed for uint/float mixing",
        
          testCase "rejects operations with non-numeric types" $
            case iHTBinary Add TypeI32 TypeBool of
              Left _ -> return ()
              Right _  -> assertFailure "Should have failed for int/bool mixing",
        
          testCase "promotion behavior for Add operation" $ do
            iHTBinary Add TypeI8 TypeI8 @?= Right TypeI8
            iHTBinary Add TypeU8 TypeU16 @?= Right TypeU16
            iHTBinary Add TypeI16 TypeI32 @?= Right TypeI32,
        
          testCase "promotion behavior for subtraction" $
            iHTBinary Sub TypeI16 TypeI32 @?= Right TypeI32,
        
          testCase "promotion behavior for division" $
            iHTBinary Div TypeU8 TypeU16 @?= Right TypeU16,

          testCase "promotion behavior for Add and Mul with TypeAny" $ do
            iHTBinary Add TypeI32 TypeAny @?= Right TypeI32
            iHTBinary Mul TypeF32 TypeAny @?= Right TypeF64,
        
          testCase "handles TypeAny in int family" $ do
            iHTBinary Add TypeI32 TypeAny @?= Right TypeI32
            iHTBinary Sub TypeAny TypeI16 @?= Right TypeI16,
        
          testCase "handles TypeAny in uint family" $ do
            iHTBinary Add TypeU32 TypeAny @?= Right TypeU32
            iHTBinary Sub TypeAny TypeU16 @?= Right TypeU16,
        
          testCase "handles TypeAny in float family" $ do
            iHTBinary Add TypeF32 TypeAny @?= Right TypeF32
            iHTBinary Sub TypeAny TypeF32 @?= Right TypeF32
        ]
    ]
