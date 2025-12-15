{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

#define TESTING_EXPORT

module IR.IRHelpersSpecs (irHelpersTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertFailure)
import Control.Monad.State (runState, execState)
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import Control.Exception (evaluate, try, SomeException(..))
import Rune.IR.IRHelpers
import Rune.IR.Nodes (GenState(..), IRType(..), IROperand(..), IRInstruction(..), IRLabel(..), IRTopLevel(..), IRGlobalValue(..))
import Rune.AST.Nodes (Type(..))
import Data.List (isInfixOf)

--
-- public
--

irHelpersTests :: TestTree
irHelpersTests =
  testGroup
    "Rune.IR.IRHelpers Specs"
    [ testAstTypeToIRType
    , testSizeOfIRType
    , testSymbolTableHelpers
    , testNamingHelpers
    , testStringGlobalHelpers
    , testFloatGlobalHelpers
    , testControlFlowHelpers
    , testOperandHelpers
    , testSelectReturnType
    , testIRTypeToAstType
    , testIsIntType
    , testIntWidth
    , testIsSigned
    , testSignedTypeOfWidth
    , testUnsignedTypeOfWidth
    , testIsFloatType
    ]

--
-- helpers
--

emptyState :: GenState
emptyState = GenState
  { gsTempCounter = 0
  , gsLabelCounter = 0
  , gsStringCounter = 0
  , gsFloatCounter = 0
  , gsGlobals = []
  , gsCurrentFunc = Nothing
  , gsSymTable = Map.empty
  , gsStructs = Map.empty
  , gsLoopStack = []
  , gsCalledFuncs = Set.empty
  , gsStringMap = Map.empty
  , gsFloatMap = Map.empty
  , gsFuncStack = HM.empty
  }

--
-- private tests
--

testAstTypeToIRType :: TestTree
testAstTypeToIRType = testGroup "astTypeToIRType"
  [ testCase "Converts primitives" $ do
      astTypeToIRType TypeI8 @?= IRI8
      astTypeToIRType TypeI16 @?= IRI16
      astTypeToIRType TypeI32 @?= IRI32
      astTypeToIRType TypeI64 @?= IRI64
      astTypeToIRType TypeU8 @?= IRU8
      astTypeToIRType TypeU16 @?= IRU16
      astTypeToIRType TypeU32 @?= IRU32
      astTypeToIRType TypeU64 @?= IRU64
      astTypeToIRType TypeF32 @?= IRF32
      astTypeToIRType TypeF64 @?= IRF64
      astTypeToIRType TypeBool @?= IRBool
      astTypeToIRType TypeChar @?= IRChar
      astTypeToIRType TypeNull @?= IRNull
  , testCase "Converts complex types" $ do
      astTypeToIRType (TypeCustom "MyStruct") @?= IRStruct "MyStruct"
      astTypeToIRType TypeString @?= IRPtr IRChar
 , testCase "Throws error on unsupported type (e.g., TypeAny)" $ do
    let criticalMsg = "Unsupported type conversion from AST to IR"
    
    result <- try @SomeException (evaluate $ astTypeToIRType TypeAny)
    
    case result of
      Left (SomeException e) -> 
        if take (length criticalMsg) (show e) == criticalMsg
          then return ()
          else assertFailure $ "Exception message did not start with critical part: " ++ criticalMsg ++ ". Full message: " ++ show e
      Right _ -> assertFailure "Expected an error for unsupported type, but no exception was thrown"
  ]

testIRTypeToAstType :: TestTree
testIRTypeToAstType = testGroup "irTypeToASTType"
  [ testCase "Converts primitives" $ do
      irTypeToASTType IRI8 @?= TypeI8
      irTypeToASTType IRI16 @?= TypeI16
      irTypeToASTType IRI32 @?= TypeI32
      irTypeToASTType IRI64 @?= TypeI64
      irTypeToASTType IRU8 @?= TypeU8
      irTypeToASTType IRU16 @?= TypeU16
      irTypeToASTType IRU32 @?= TypeU32
      irTypeToASTType IRU64 @?= TypeU64
      irTypeToASTType IRF32 @?= TypeF32
      irTypeToASTType IRF64 @?= TypeF64
      irTypeToASTType IRBool @?= TypeBool
      irTypeToASTType IRChar @?= TypeChar
      irTypeToASTType IRNull @?= TypeNull
  , testCase "Converts complex types" $ do
      irTypeToASTType (IRStruct "MyStruct") @?= TypeCustom "MyStruct"
      irTypeToASTType (IRPtr IRChar) @?= TypeString
      irTypeToASTType (IRPtr (IRStruct "MyStruct")) @?= TypeCustom "MyStruct"
      irTypeToASTType (IRPtr IRI32) @?= TypeAny
  ]

testSizeOfIRType :: TestTree
testSizeOfIRType = testGroup "sizeOfIRType"
  [ testCase "Integer sizes" $ do
      sizeOfIRType IRI8 @?= 1
      sizeOfIRType IRI16 @?= 2
      sizeOfIRType IRI32 @?= 4
      sizeOfIRType IRI64 @?= 8
  , testCase "Unsigned Integer sizes" $ do
      sizeOfIRType IRU8 @?= 1
      sizeOfIRType IRU16 @?= 2
      sizeOfIRType IRU32 @?= 4
      sizeOfIRType IRU64 @?= 8
  , testCase "Float sizes" $ do
      sizeOfIRType IRF32 @?= 4
      sizeOfIRType IRF64 @?= 8
  , testCase "Other sizes" $ do
      sizeOfIRType IRBool @?= 1
      sizeOfIRType IRChar @?= 1
      sizeOfIRType IRNull @?= 8
      sizeOfIRType (IRPtr IRI32) @?= 8
      sizeOfIRType (IRStruct "S") @?= 8
  ]

testSignedTypeOfWidth :: TestTree
testSignedTypeOfWidth = testGroup "signedTypeOfWidth"
  [ testCase "Returns correct signed types" $ do
      signedTypeOfWidth 8 @?= IRI8
      signedTypeOfWidth 16 @?= IRI16
      signedTypeOfWidth 32 @?= IRI32
      signedTypeOfWidth 64 @?= IRI64
  , testCase "Default case" $ do
      signedTypeOfWidth 7 @?= IRI32
      signedTypeOfWidth 128 @?= IRI32
  ]

testUnsignedTypeOfWidth :: TestTree
testUnsignedTypeOfWidth = testGroup "unsignedTypeOfWidth"
  [ testCase "Returns correct unsigned types" $ do
      unsignedTypeOfWidth 8 @?= IRU8
      unsignedTypeOfWidth 16 @?= IRU16
      unsignedTypeOfWidth 32 @?= IRU32
      unsignedTypeOfWidth 64 @?= IRU64
  , testCase "Default case" $ do
      unsignedTypeOfWidth 7 @?= IRU32
      unsignedTypeOfWidth 128 @?= IRU32
  ]

testSymbolTableHelpers :: TestTree
testSymbolTableHelpers = testGroup "Symbol Table Helpers"
  [ testCase "registerVar adds to symbol table" $
      let op = IRTemp "t1" IRI32
          state = execState (registerVar "x" op IRI32) emptyState
      in Map.lookup "x" (gsSymTable state) @?= Just (op, IRI32)
  
  , testCase "registerCall adds to called funcs set" $
      let state = execState (registerCall "myFunc") emptyState
      in Set.member "myFunc" (gsCalledFuncs state) @?= True
  ]

testNamingHelpers :: TestTree
testNamingHelpers = testGroup "Naming Helpers"
  [ testCase "newTemp increments counter and returns name" $
      let (name, state) = runState (newTemp "tmp" IRI32) emptyState
      in do
        name @?= "tmp0"
        gsTempCounter state @?= 1

  , testCase "nextLabelIndex increments counter" $
      let (idx, state) = runState nextLabelIndex emptyState
      in do
        idx @?= 0
        gsLabelCounter state @?= 1

  , testCase "makeLabel formats correctly" $
      makeLabel "loop" 42 @?= IRLabel ".L.loop42"
  
  , testCase "mangleMethodName combines strings" $
      mangleMethodName "Struct" "method" @?= "Struct_method"
  ]

testStringGlobalHelpers :: TestTree
testStringGlobalHelpers = testGroup "String Global Helpers"
  [ testCase "newStringGlobal creates new global if not exists" $
      let (name, state) = runState (newStringGlobal "hello") (emptyState { gsCurrentFunc = Just "main" })
      in do
        name @?= "str_main0"
        gsStringCounter state @?= 1
        Map.lookup "hello" (gsStringMap state) @?= Just "str_main0"
        length (gsGlobals state) @?= 1
        case gsGlobals state of
          (IRGlobalDef n (IRGlobalStringVal v) : _) -> do
            n @?= "str_main0"
            v @?= "hello"
          _ -> assertFailure "Expected IRGlobalDef with string value"

  , testCase "newStringGlobal returns existing name if exists" $
      let state0 = emptyState { gsStringMap = Map.singleton "hello" "str_existing" }
          (name, state) = runState (newStringGlobal "hello") state0
      in do
        name @?= "str_existing"
        gsStringCounter state @?= 0
        length (gsGlobals state) @?= 0

  , testCase "genFormatString wraps newStringGlobal" $
      let (result, _) = runState (genFormatString "fmt") (emptyState { gsCurrentFunc = Just "main" })
      in case result of
        (instrs, op) -> do
          instrs @?= []
          case op of
            IRGlobal name typ -> do
              name @?= "str_main0"
              typ @?= IRPtr IRChar
            _ -> assertFailure "Expected IRGlobal operand"

  , testCase "newStringGlobal uses 'global' prefix when outside a function" $
      let (name, state) = runState (newStringGlobal "top") emptyState
      in do
        name @?= "str_global0"
        Map.lookup "top" (gsStringMap state) @?= Just "str_global0"
        case gsGlobals state of
          (IRGlobalDef n (IRGlobalStringVal v) : _) -> do
            n @?= "str_global0"
            v @?= "top"
          _ -> assertFailure "Expected IRGlobalDef with string value for top-level string"
  ]

testFloatGlobalHelpers :: TestTree
testFloatGlobalHelpers = testGroup "Float Global Helpers"
  [ testCase "newFloatGlobal creates new global when not interned" $
      let (name, state) = runState (newFloatGlobal 3.14 IRF32) emptyState
      in do
        name @?= "f32_global0"
        gsFloatCounter state @?= 1
        Map.lookup (3.14, IRF32) (gsFloatMap state) @?= Just "f32_global0"
        case gsGlobals state of
          (IRGlobalDef n (IRGlobalFloatVal v t) : _) -> do
            n @?= "f32_global0"
            v @?= 3.14
            t @?= IRF32
          _ -> assertFailure "Expected IRGlobalDef with float value"

  , testCase "newFloatGlobal reuses existing label when value interned" $
      let initial =
            emptyState
              { gsFloatCounter = 1
              , gsFloatMap = Map.singleton (2.71, IRF32) "f32_global0"
              , gsGlobals = [IRGlobalDef "f32_global0" (IRGlobalFloatVal 2.71 IRF32)]
              }
          (name, state) = runState (newFloatGlobal 2.71 IRF32) initial
      in do
        name @?= "f32_global0"
        gsFloatCounter state @?= 1
        gsGlobals state @?= gsGlobals initial
  ]

testControlFlowHelpers :: TestTree
testControlFlowHelpers = testGroup "Control Flow Helpers"
  [ testCase "endsWithRet detects return" $
      endsWithRet [IRALLOC "x" IRI32, IRRET Nothing] @?= True
  , testCase "endsWithRet detects jump" $
      endsWithRet [IRJUMP (IRLabel "L1")] @?= True
  , testCase "endsWithRet returns False otherwise" $
      endsWithRet [IRALLOC "x" IRI32] @?= False
  , testCase "endsWithRet handles empty list" $
      endsWithRet [] @?= False
  
  , testCase "Loop context operations" $
      let startLbl = IRLabel "start"
          endLbl = IRLabel "end"
          op = do
            pushLoopContext startLbl endLbl
            curr1 <- getCurrentLoop
            popLoopContext
            curr2 <- getCurrentLoop
            return (curr1, curr2)
          ((l1, l2), _) = runState op emptyState
      in do
        l1 @?= Just (startLbl, endLbl)
        l2 @?= Nothing
  ]

testOperandHelpers :: TestTree
testOperandHelpers = testGroup "Operand Helpers"
  [ testCase "getOperandType extracts type" $ do
      getOperandType (IRTemp "t" IRI32) @?= Just IRI32
      getOperandType (IRParam "p" IRF64) @?= Just IRF64
      getOperandType (IRGlobal "g" IRU8) @?= Just IRU8
      getOperandType (IRConstInt 5) @?= Just IRI32
      getOperandType (IRConstFloat 1.0) @?= Just IRF32
      getOperandType (IRConstChar 'c') @?= Just IRChar
      getOperandType (IRConstBool True) @?= Just IRBool
      getOperandType (IRConstNull) @?= Just IRNull

  , testCase "getCommonType promotes types" $ do
      getCommonType (IRTemp "a" IRI32) (IRTemp "b" IRI32) @?= IRI32
      
      getCommonType (IRTemp "a" IRF32) (IRTemp "b" IRF64) @?= IRF64
      getCommonType (IRTemp "a" IRI32) (IRTemp "b" IRF32) @?= IRF32
      
      getCommonType (IRTemp "a" IRI8) (IRTemp "b" IRI32) @?= IRI32
      getCommonType (IRTemp "a" IRI64) (IRTemp "b" IRI32) @?= IRI64
      
      getCommonType (IRTemp "a" IRU8) (IRTemp "b" IRU16) @?= IRU16

      getCommonType (IRTemp "a" IRI32) (IRTemp "b" IRU32) @?= IRI32
      getCommonType (IRTemp "a" IRI16) (IRTemp "b" IRU32) @?= IRI32
      getCommonType (IRTemp "a" IRU16) (IRTemp "b" IRI8) @?= IRI16

      getCommonType (IRConstBool True) (IRConstBool False) @?= IRBool
      getCommonType (IRConstChar 'a') (IRConstChar 'b') @?= IRChar

      getCommonType (IRConstInt 1) (IRTemp "b" IRI32) @?= IRI32
      getCommonType (IRConstBool True) (IRConstChar 'a') @?= IRI32
  ]

testIsIntType :: TestTree
testIsIntType = testGroup "isIntType"
  [ testCase "Identifies signed integers" $ do
      isIntType IRI8 @?= True
      isIntType IRI16 @?= True
      isIntType IRI32 @?= True
      isIntType IRI64 @?= True
  , testCase "Identifies unsigned integers" $ do
      isIntType IRU8 @?= True
      isIntType IRU16 @?= True
      isIntType IRU32 @?= True
      isIntType IRU64 @?= True
  , testCase "Rejects non-integer types" $ do
      isIntType IRF32 @?= False
      isIntType IRF64 @?= False
      isIntType IRBool @?= False
      isIntType IRChar @?= False
      isIntType IRNull @?= False
      isIntType (IRPtr IRI32) @?= False
      isIntType (IRStruct "S") @?= False
  ]

testIntWidth :: TestTree
testIntWidth = testGroup "intWidth"
  [ testCase "Returns correct widths for signed integers" $ do
      intWidth IRI8 @?= 8
      intWidth IRI16 @?= 16
      intWidth IRI32 @?= 32
      intWidth IRI64 @?= 64
  , testCase "Returns correct widths for unsigned integers" $ do
      intWidth IRU8 @?= 8
      intWidth IRU16 @?= 16
      intWidth IRU32 @?= 32
      intWidth IRU64 @?= 64
  , testCase "Default case" $ do
      intWidth IRF32 @?= 0
      intWidth IRF64 @?= 0
      intWidth IRBool @?= 0
      intWidth IRChar @?= 0
      intWidth IRNull @?= 0
      intWidth (IRPtr IRI32) @?= 0
      intWidth (IRStruct "S") @?= 0
  ]

testIsSigned :: TestTree
testIsSigned = testGroup "isSigned"
  [ testCase "Identifies signed integers" $ do
      isSigned IRI8 @?= True
      isSigned IRI16 @?= True
      isSigned IRI32 @?= True
      isSigned IRI64 @?= True
  , testCase "Identifies unsigned integers" $ do
      isSigned IRU8 @?= False
      isSigned IRU16 @?= False
      isSigned IRU32 @?= False
      isSigned IRU64 @?= False
  , testCase "Rejects non-integer types" $ do
      isSigned IRF32 @?= False
      isSigned IRF64 @?= False
      isSigned IRBool @?= False
      isSigned IRChar @?= False
      isSigned IRNull @?= False
      isSigned (IRPtr IRI32) @?= False
      isSigned (IRStruct "S") @?= False
  , testCase "Empty type case" $ do
      isSigned IRNull @?= False
  ]

testSelectReturnType :: TestTree
testSelectReturnType = testGroup "selectReturnType"
  [ testCase "Returns correct return type for existing function" $
      let fs = HM.fromList [("f", [(TypeI32, [TypeI32, TypeF32])])]
          args = [IRI32, IRF32]
      in selectReturnType fs "f" args @?= IRI32
  
  , testCase "Maps IR types back to AST types correctly" $
      let fs = HM.fromList [("g", [(TypeU32, [TypeU32, TypeAny, TypeCustom "S"])])]
          args = [IRU32, IRPtr IRI32, IRPtr (IRStruct "S")]
      in selectReturnType fs "g" args @?= IRU32 
  , testCase "Select signature Nothing" $
      let fs = HM.fromList [("h", [(TypeF64, [TypeF32])])]
          args = [IRF32]
      in selectReturnType fs "h" args @?= IRF64
  , testCase "Throws error and checks message content for coverage" $ do
        let fs = HM.fromList [("mismatched", [(TypeI32, [TypeI32, TypeF32])])]
            funcName = "mismatched"
            args = [IRI32, IRU64, IRI8]
            criticalMsg = "Semantic error: No matching signature found for function call: mismatched with arguments: [i32,u64,i8]"
        
        result <- try @SomeException (evaluate $ selectReturnType fs funcName args)
        
        case result of
          Left (SomeException e) -> 
            if criticalMsg `isInfixOf` show e
              then return ()
              else assertFailure $ "Exception message did not contain critical part: " ++ criticalMsg ++ ". Full message: " ++ show e
          Right _ -> assertFailure "Expected a semantic error for no matching signature, but no exception was thrown"
    ]

testIsFloatType :: TestTree
testIsFloatType = testGroup "isFloatType"
  [ testCase "Identifies float types" $ do
      isFloatType IRF32 @?= True
      isFloatType IRF64 @?= True
  , testCase "Rejects non-float types" $ do
      isFloatType IRI32 @?= False
      isFloatType IRU32 @?= False
      isFloatType IRBool @?= False
      isFloatType IRChar @?= False
      isFloatType IRNull @?= False
      isFloatType (IRPtr IRI32) @?= False
      isFloatType (IRStruct "S") @?= False
  ]
