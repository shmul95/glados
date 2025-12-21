{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
#define TESTING_EXPORT

module IR.Generator.Expression.Call.ShowSpecs (showCallTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool, assertFailure)
import Control.Monad.State (runState)
import Control.Monad.Except (runExceptT)
import Data.List (isPrefixOf)
import qualified Data.Set as Set
import TestHelpers (dummyPos)
import Rune.IR.Generator.Expression.Call.Show
import Rune.IR.Nodes (IRType(..), IROperand(..), IRInstruction(..), IRTopLevel(..), IRFunction(..), IRGlobalValue(..), GenState(..), IRGen)
import Rune.AST.Nodes (Expression(..))
import IR.TestUtils (emptyState)
import Rune.IR.IRHelpers (newFloatGlobal)

--
-- public
--

showCallTests :: TestTree
showCallTests = testGroup "Rune.IR.Generator.Expression.Call.Show"
  [ test_show_bool_generation_details
  , test_show_char_generation_details
  , test_show_i64_generation_details
  , test_show_f64_generation_details
  , test_show_struct_generation_details
  , test_show_string_generation_details
  , test_get_show_func_struct
  , test_get_show_func_ptr_struct
  , test_get_show_func_primitive
  , test_get_format_specifier_signed_int
  , test_get_format_specifier_unsigned_int
  , test_get_format_specifier_float
  , test_get_format_specifier_char_and_string
  , test_get_format_specifier_null_and_unknown
  , test_prepare_addr_struct_temp
  , test_prepare_addr_struct_ptr
  , test_prepare_addr_non_struct
  , test_ensure_show_bool_func_definition
  , test_override_exists
  , test_mk_show_bool_func_content
  ]

--
-- helpers
--

isCall :: String -> IRInstruction -> Bool
isCall name (IRCALL _ n _ _) = n == name
isCall _ _ = False

isShowBoolDef :: IRTopLevel -> Bool
isShowBoolDef (IRFunctionDef f) = irFuncName f == "show_bool"
isShowBoolDef _ = False

isAddr :: IRInstruction -> Bool
isAddr (IRADDR _ _ _) = True
isAddr _ = False

genExprSimple :: Expression -> IRGen ([IRInstruction], IROperand, IRType)
genExprSimple (ExprLitInt _ n) = return ([], IRConstInt n, IRI64)
genExprSimple (ExprLitBool _ b) = return ([], IRConstBool b, IRBool)
genExprSimple (ExprLitChar _ c) = return ([], IRConstChar c, IRChar)
genExprSimple _ = error "Unexpected expression in genExprSimple"

genExprFloat :: Expression -> IRGen ([IRInstruction], IROperand, IRType)
genExprFloat (ExprLitFloat _ n) = do
    gName <- newFloatGlobal n IRF64
    return ([], IRGlobal gName IRF64, IRF64)
genExprFloat _ = error "Unexpected expression in genExprFloat"

genExprString :: Expression -> IRGen ([IRInstruction], IROperand, IRType)
genExprString (ExprLitString _ _) = do
    let strOp = IRPtr IRChar
    return ([], IRGlobal "str_global0" strOp, strOp)
genExprString _ = error "Unexpected expression in genExprString"

genExprStruct :: Expression -> IRGen ([IRInstruction], IROperand, IRType)
genExprStruct (ExprVar _ "s") = return ([], IRTemp "s" (IRStruct "Vec"), IRStruct "Vec")
genExprStruct _ = error "Unexpected expression in genExprStruct"


--
-- private
--

test_show_bool_generation_details :: TestTree
test_show_bool_generation_details = testCase "Boolean generates call to show_bool and adds it to globals" $ do
    case runState (runExceptT $ genShowCall genExprSimple (ExprLitBool dummyPos True)) emptyState of
      (Left err, _) -> assertFailure $ "Unexpected error: " ++ err
      (Right (instrs, _, typ), state) -> do
        typ @?= IRNull
        assertBool "Should contain call to show_bool" $ any (isCall "show_bool") instrs
        assertBool "show_bool function should be added to globals" $ any isShowBoolDef (gsGlobals state)

test_show_char_generation_details :: TestTree
test_show_char_generation_details = testCase "IRChar generates call to putchar" $ do
    case runState (runExceptT $ genShowCall genExprSimple (ExprLitChar dummyPos 'x')) emptyState of
      (Left err, _) -> assertFailure $ "Unexpected error: " ++ err
      (Right (instrs, _, typ), _) -> do
        typ @?= IRNull
        assertBool "Should contain call to putchar" $ any (isCall "putchar") instrs

test_show_i64_generation_details :: TestTree
test_show_i64_generation_details = testCase "IRI64 generates printf with %ld format string" $ do
    case runState (runExceptT $ genShowCall genExprSimple (ExprLitInt dummyPos 123)) emptyState of
      (Left err, _) -> assertFailure $ "Unexpected error: " ++ err
      (Right (instrs, _, typ), state) -> do
        typ @?= IRNull
        assertBool "Should contain call to printf" $ any (isCall "printf") instrs
        
        let formatStrGlobals = [v | IRGlobalDef _ (IRGlobalStringVal v) <- gsGlobals state]
        assertBool "Should register %ld format string" $ any ("%ld" `isPrefixOf`) formatStrGlobals
        
        case filter (isCall "printf") instrs of
          [IRCALL _ _ args _] -> assertBool "Should have 2 arguments in CALL (fmt, val)" $ length args == 2
          _ -> assertFailure "Expected exactly one printf call"

test_show_f64_generation_details :: TestTree
test_show_f64_generation_details = testCase "IRF64 generates printf with %lf format string" $ do
    case runState (runExceptT $ genShowCall genExprFloat (ExprLitFloat dummyPos 3.14)) emptyState of
      (Left err, _) -> assertFailure $ "Unexpected error: " ++ err
      (Right (instrs, _, typ), state) -> do
        typ @?= IRNull
        assertBool "Should contain call to printf" $ any (isCall "printf") instrs
        
        let formatStrGlobals = [v | IRGlobalDef _ (IRGlobalStringVal v) <- gsGlobals state]
        assertBool "Should register %lf format string" $ any ("%lf" `isPrefixOf`) formatStrGlobals

test_show_struct_generation_details :: TestTree
test_show_struct_generation_details = testCase "IRStruct generates call to show_Struct and IRADDR" $ do
    case runState (runExceptT $ genShowCall genExprStruct (ExprVar dummyPos "s")) emptyState of
      (Left err, _) -> assertFailure $ "Unexpected error: " ++ err
      (Right (instrs, _, typ), _) -> do
        typ @?= IRNull
        assertBool "Should contain IRADDR for struct pointer" $ any isAddr instrs
        assertBool "Should contain call to show_Vec" $ any (isCall "show_Vec") instrs

test_show_string_generation_details :: TestTree
test_show_string_generation_details = testCase "IRPtr IRChar (string) generates printf with %s format string" $ do
    case runState (runExceptT $ genShowCall genExprString (ExprLitString dummyPos "test")) emptyState of
      (Left err, _) -> assertFailure $ "Unexpected error: " ++ err
      (Right (instrs, _, typ), state) -> do
        typ @?= IRNull
        assertBool "Should contain call to printf" $ any (isCall "printf") instrs
        
        let formatStrGlobals = [v | IRGlobalDef _ (IRGlobalStringVal v) <- gsGlobals state]
        assertBool "Should register %s format string" $ any ("%s" `isPrefixOf`) formatStrGlobals


test_get_show_func_struct :: TestTree
test_get_show_func_struct = testCase "getShowFunc returns show_StructName for IRStruct" $
  getShowFunc (IRTemp "s" (IRStruct "Point")) (IRStruct "Point") @?= "show_Point"

test_get_show_func_ptr_struct :: TestTree
test_get_show_func_ptr_struct = testCase "getShowFunc returns show_StructName for IRPtr IRStruct" $
  getShowFunc (IRTemp "p" (IRPtr (IRStruct "Vec"))) (IRPtr (IRStruct "Vec")) @?= "show_Vec"

test_get_show_func_primitive :: TestTree
test_get_show_func_primitive = testCase "getShowFunc returns printf for primitives" $
  getShowFunc (IRConstInt 42) IRI32 @?= "printf"

test_get_format_specifier_signed_int :: TestTree
test_get_format_specifier_signed_int = testCase "getFormatSpecifier returns correct format for signed int types" $ do
  getFormatSpecifier (IRConstInt 0) IRI8 @?= Just "%hhd"
  getFormatSpecifier (IRConstInt 0) IRI16 @?= Just "%hd"
  getFormatSpecifier (IRConstInt 0) IRI32 @?= Just "%d"
  getFormatSpecifier (IRConstInt 0) IRI64 @?= Just "%ld"

test_get_format_specifier_unsigned_int :: TestTree
test_get_format_specifier_unsigned_int = testCase "getFormatSpecifier returns correct format for unsigned int types" $ do
  getFormatSpecifier (IRConstInt 0) IRU8 @?= Just "%hhu"
  getFormatSpecifier (IRConstInt 0) IRU16 @?= Just "%hu"
  getFormatSpecifier (IRConstInt 0) IRU32 @?= Just "%u"
  getFormatSpecifier (IRConstInt 0) IRU64 @?= Just "%lu"

test_get_format_specifier_float :: TestTree
test_get_format_specifier_float = testCase "getFormatSpecifier returns correct format for float types" $ do
  getFormatSpecifier (IRConstFloat 0.0) IRF32 @?= Just "%f"
  getFormatSpecifier (IRConstFloat 0.0) IRF64 @?= Just "%lf"

test_get_format_specifier_char_and_string :: TestTree
test_get_format_specifier_char_and_string = testCase "getFormatSpecifier returns correct format for char and string" $ do
  getFormatSpecifier (IRConstChar 'a') IRChar @?= Just "%c"
  getFormatSpecifier (IRGlobal "s" (IRPtr IRChar)) (IRPtr IRChar) @?= Just "%s"

test_get_format_specifier_null_and_unknown :: TestTree
test_get_format_specifier_null_and_unknown = testCase "getFormatSpecifier returns correct format for null and Nothing for unknown types" $ do
  getFormatSpecifier IRConstNull IRNull @?= Just "(null)"
  getFormatSpecifier (IRConstInt 0) (IRStruct "Unknown") @?= Nothing

test_prepare_addr_struct_temp :: TestTree
test_prepare_addr_struct_temp = testCase "prepareAddr adds IRADDR for IRStruct temp" $ do
    let (instrs, op) = prepareAddr (IRTemp "s" (IRStruct "Point")) (IRStruct "Point")

    assertBool "Should have IRADDR" $ not $ null instrs
    case op of
      IRTemp name (IRPtr (IRStruct "Point")) -> assertBool "Name should be prefixed with addr_" $ "addr_" `isPrefixOf` name
      _ -> assertFailure "Expected addr temp for struct"

test_prepare_addr_struct_ptr :: TestTree
test_prepare_addr_struct_ptr = testCase "prepareAddr adds IRADDR for IRPtr IRStruct temp" $ do
    let (instrs, op) = prepareAddr (IRTemp "p" (IRPtr (IRStruct "Vec"))) (IRPtr (IRStruct "Vec"))

    assertBool "Should have IRADDR" $ not $ null instrs
    case op of
      IRTemp name (IRPtr (IRPtr (IRStruct "Vec"))) -> assertBool "Name should be prefixed with addr_" $ "addr_" `isPrefixOf` name
      _ -> assertFailure "Expected double pointer temp for struct pointer"

test_prepare_addr_non_struct :: TestTree
test_prepare_addr_non_struct = testCase "prepareAddr passes through non-struct operands" $ do
    let (instrs, op) = prepareAddr (IRConstInt 42) IRI32

    instrs @?= []
    op @?= IRConstInt 42

test_ensure_show_bool_func_definition :: TestTree
test_ensure_show_bool_func_definition = testCase "ensureShowBoolFunc adds show_bool definition and registers printf" $ do
    case runState (runExceptT ensureShowBoolFunc) emptyState of
      (Left err, _) -> assertFailure $ "Unexpected error: " ++ err
      (Right _, state) -> do
        assertBool "show_bool should be in globals" $ any isShowBoolDef (gsGlobals state)
        assertBool "printf should be registered as external" $ Set.member "printf" (gsCalledFuncs state)

test_override_exists :: TestTree
test_override_exists = testCase "overrideExists detects if IRTopLevel is a function with the target name" $ do
    let func = IRFunction "show_int" [] Nothing []
    let funcDef = IRFunctionDef func
    let extern = IRExtern "printf"
    
    overrideExists funcDef "show_int" @?= True
    overrideExists funcDef "other_func" @?= False
    overrideExists extern "printf" @?= False

test_mk_show_bool_func_content :: TestTree
test_mk_show_bool_func_content = testCase "mkShowBoolFunc" $ do
  case runState (runExceptT mkShowBoolFunc) emptyState of
    (Left err, _) -> assertFailure $ "Unexpected error: " ++ err
    (Right _, state) ->
      assertBool "printf should be registered as external" $ Set.member "printf" (gsCalledFuncs state)
