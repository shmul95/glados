module Backend.X86_64.CodegenSpecs (codegenTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)
import qualified Data.Map.Strict as Map
import Rune.Backend.X86_64.Codegen
import Rune.IR.Nodes

--
-- public
--

codegenTests :: TestTree
codegenTests = testGroup "Rune.Backend.X86_64.Codegen"
  [
    commaSepTests,
    showStaticOperandTests,
    getDataDirectiveTests,
    isStaticOperandTests,
    collectStaticArraysTests,
    emitExternsTests,
    emitRoDataSectionTests,
    emitRmWarningTests,
    emitFunctionPrologueTests,
    emitFunctionEpilogueTests,
    emitParametersTests,
    emitAssignTests,
    emitCallTests,
    emitRetTests,
    emitDerefTests,
    emitAllocArrayTests,
    emitAllocArrayOnStackTests,
    emitGetElemTests,
    emitSetElemTests,
    emitIncDecTests,
    emitIncDecHelpersTests,
    emitAddrTests,
    emitConditionalJumpTests,
    saveCallResultTests,
    setupCallArgsTests
  ]

--
-- private
--

setupCallArgsTests :: TestTree
setupCallArgsTests = testGroup "setupCallArgs"
  [
    testCase "mix int and float arguments" $
      let sm = Map.fromList [("a", -4), ("b", -8), ("c", -16)]
          args = [IRTemp "a" IRI32, IRTemp "b" IRF32, IRTemp "c" IRI64]
          result = setupCallArgs sm args
      in assertBool "should map to correct registers" $
           any (== "    movsxd rdi, dword [rbp-4]") result &&
           any (== "    movss xmm0, dword [rbp-8]") result &&
           any (== "    mov rsi, qword [rbp-16]") result
  , testCase "exhaust all integer registers and ignore excess" $
      let indices = [1..7 :: Int]
          sm = Map.fromList $ map (\i -> ("v" ++ show i, -8 * i)) indices
          args = map (\i -> IRTemp ("v" ++ show i) IRI64) indices
          result = setupCallArgs sm args
      in assertBool "should use 6 registers and skip the 7th" $
           any (== "    mov rdi, qword [rbp-8]") result &&
           any (== "    mov rsi, qword [rbp-16]") result &&
           any (== "    mov rdx, qword [rbp-24]") result &&
           any (== "    mov rcx, qword [rbp-32]") result &&
           any (== "    mov r8, qword [rbp-40]") result &&
           any (== "    mov r9, qword [rbp-48]") result &&
           not (any (== "    mov r10, qword [rbp-56]") result)
  , testCase "exhaust all float registers and ignore excess" $
      let indices = [1..9 :: Int]
          sm = Map.fromList $ map (\i -> ("f" ++ show i, -8 * i)) indices
          args = map (\i -> IRTemp ("f" ++ show i) IRF64) indices
          result = setupCallArgs sm args
      in assertBool "should use 8 xmm registers and skip the 9th" $
           any (== "    movsd xmm0, qword [rbp-8]") result &&
           any (== "    movsd xmm7, qword [rbp-64]") result &&
           not (any (== "    movsd xmm8, qword [rbp-72]") result)
  , testCase "handle mixed overflow" $
      let iIdx = [1..7 :: Int]
          fIdx = [1..9 :: Int]
          sm = Map.fromList $ map (\i -> ("i" ++ show i, -8 * i)) iIdx 
                           ++ map (\i -> ("f" ++ show i, -64 - 8 * i)) fIdx
          args = map (\i -> IRTemp ("i" ++ show i) IRI64) iIdx
                  ++ map (\i -> IRTemp ("f" ++ show i) IRF64) fIdx
          result = setupCallArgs sm args
      in assertBool "should handle both limits correctly" $
           any (== "    mov r9, qword [rbp-48]") result &&
           any (== "    movsd xmm7, qword [rbp-128]") result
  ]

saveCallResultTests :: TestTree
saveCallResultTests = testGroup "saveCallResult"
  [
    testCase "no destination" $
      let result = saveCallResult Map.empty "" Nothing
      in assertBool "should be empty" $ null result
  , testCase "integer return" $
      let sm = Map.fromList [("res", -8)]
          result = saveCallResult sm "res" (Just IRI32)
      in assertBool "should store eax to stack" $
           any (== "    mov dword [rbp-8], eax") result
  , testCase "float return" $
      let sm = Map.fromList [("fres", -8)]
          result = saveCallResult sm "fres" (Just IRF32)
      in assertBool "should store xmm0 to stack" $
           any (== "    movss dword [rbp-8], xmm0") result
  , testCase "void return" $
      let sm = Map.fromList [("vres", -8)]
          result = saveCallResult sm "vres" Nothing
      in assertBool "should store rax to stack" $
           any (== "    mov qword [rbp-8], rax") result
  , testCase "f64 return" $
      let sm = Map.fromList [("dres", -16)]
          result = saveCallResult sm "dres" (Just IRF64)
      in assertBool "should store xmm0 to stack" $
           any (== "    movsd qword [rbp-16], xmm0") result
  ]

collectStaticArraysTests :: TestTree
collectStaticArraysTests = testGroup "collectStaticArrays"
  [
    testCase "collect static array" $
      let values = [IRConstInt 1, IRConstInt 2]
          instr = IRALLOC_ARRAY "arr" IRI32 values
          func = IRFunction "main" [] Nothing [instr]
          result = collectStaticArrays [func]
      in case result of
           [(n, t, v)] -> assertBool "content match" $ n == "main_arr_lit" && t == IRI32 && v == values
           _           -> assertBool "expected exactly one static array" False
  , testCase "ignore dynamic array" $
      let instr = IRALLOC_ARRAY "arr" IRI32 [IRTemp "t" IRI32]
          func = IRFunction "main" [] Nothing [instr]
      in assertBool "should be empty" $ null (collectStaticArrays [func])
  ]

emitParametersTests :: TestTree
emitParametersTests = testGroup "emitParameters"
  [
    testCase "mix int and float parameters" $
      let params = [("a", IRI32), ("b", IRF32), ("c", IRI64)]
          sm = Map.fromList [("a", -4), ("b", -8), ("c", -16)]
          result = emitParameters params sm
      in assertBool "should map to correct registers" $
           any (== "    mov dword [rbp-4], edi") result &&
           any (== "    movss dword [rbp-8], xmm0") result &&
           any (== "    mov qword [rbp-16], rsi") result
  ]

emitCallTests :: TestTree
emitCallTests = testGroup "emitCall"
  [
    testCase "call with arguments and return" $
      let sm = Map.fromList [("arg1", -4), ("res", -8)]
          args = [IRTemp "arg1" IRI32]
          result = emitCall sm "res" "my_func" args (Just IRI32)
      in assertBool "should setup args and call" $
           any (== "    movsxd rdi, dword [rbp-4]") result &&
           any (== "    call my_func") result &&
           any (== "    mov dword [rbp-8], eax") result
  , testCase "printf float fixup" $
      let sm = Map.fromList [("f", -8)]
          args = [IRTemp "f" IRF32]
          result = emitCall sm "" "printf" args Nothing
      in assertBool "should convert float to double for printf" $
           any (== "    cvtss2sd xmm0, xmm0") result &&
           any (== "    mov eax, 1") result
  ]

emitDerefTests :: TestTree
emitDerefTests = testGroup "emitDeref"
  [
    testCase "dereference pointer" $
      let sm = Map.fromList [("ptr", -8), ("dst", -12)]
          result = emitDeref sm "dst" (IRTemp "ptr" (IRPtr IRI32)) IRI32
      in assertBool "should load rax then deref" $
           any (== "    mov rax, qword [rbp-8]") result &&
           any (== "    mov eax, dword [rax]") result &&
           any (== "    mov dword [rbp-12], eax") result
  ]



emitAllocArrayTests :: TestTree
emitAllocArrayTests = testGroup "emitAllocArray"
  [ testCase "static array path" $
      let sm = Map.fromList [("arr", -8)]
          values = [IRConstInt 10, IRConstNull]
          result = emitAllocArray sm "fn" "arr" IRI64 values
      in assertBool "should emit static mov" $
           any (== "    mov rax, fn_arr_lit") result &&
           any (== "    mov qword [rbp-8], rax") result
  , testCase "dynamic array path" $
      let sm = Map.fromList [("arr", -8), ("arr_data", -32)]
          values = [IRTemp "v" IRI32]
          result = emitAllocArray sm "fn" "arr" IRI32 values
      in assertBool "should call stack allocation" $
           any (== "    lea rax, [rbp-32]") result
  ]


emitAllocArrayOnStackTests :: TestTree
emitAllocArrayOnStackTests = testGroup "emitAllocArrayOnStack"
  [ testCase "stack allocation logic" $
      let sm = Map.fromList [("arr", -8), ("arr_data", -24), ("v", -12)]
          values = [IRConstInt 1, IRTemp "v" IRI32]
          arrType = IRArray IRI32 3
          result = emitAllocArrayOnStack sm "arr" IRI32 values arrType
      in assertBool "should setup ptr, store values, and sentinel" $
           any (== "    lea rax, [rbp-24]") result &&
           any (== "    mov rax, 1") result &&
           any (== "    mov dword [rbp-24], eax") result &&
           any (== "    mov eax, dword [rbp-12]") result &&
           any (== "    mov dword [rbp-20], eax") result &&
           any (== "    mov dword [rbp-16], 0") result
  ]


emitGetElemTests :: TestTree
emitGetElemTests = testGroup "emitGetElem"
  [
    testCase "array access IRI32" $
      let sm = Map.fromList [("arr", -8), ("idx", -16), ("res", -20)]
          target = IRTemp "arr" (IRPtr (IRArray IRI32 5))
          index = IRTemp "idx" IRI32
          result = emitGetElem sm "res" target index IRI32
      in assertBool "should load base, extend index, imul, load elem and store" $
           any (== "    mov rdi, qword [rbp-8]") result &&
           any (== "    movsxd rsi, dword [rbp-16]") result &&
           any (== "    imul rsi, 4") result &&
           any (== "    mov eax, dword [rdi + rsi]") result &&
           any (== "    mov dword [rbp-20], eax") result
  , testCase "array access IRI8" $
      let sm = Map.fromList [("arr", -8), ("idx", -16), ("res", -17)]
          target = IRTemp "arr" (IRPtr (IRArray IRI8 10))
          index = IRTemp "idx" IRI32
          result = emitGetElem sm "res" target index IRI8
      in assertBool "should handle byte-sized elements and registers" $
           any (== "    imul rsi, 1") result &&
           any (== "    mov al, byte [rdi + rsi]") result &&
           any (== "    mov byte [rbp-17], al") result
  ]


emitSetElemTests :: TestTree
emitSetElemTests = testGroup "emitSetElem"
  [
    testCase "array store IRI32" $
      let sm = Map.fromList [("arr", -8), ("idx", -16), ("val", -20)]
          target = IRTemp "arr" (IRPtr (IRArray IRI32 5))
          index = IRTemp "idx" IRI32
          value = IRTemp "val" IRI32
          result = emitSetElem sm target index value
      in assertBool "should load base, extend index, imul, load value and store" $
           any (== "    mov rdi, qword [rbp-8]") result &&
           any (== "    movsxd rsi, dword [rbp-16]") result &&
           any (== "    imul rsi, 4") result &&
           any (== "    mov eax, dword [rbp-20]") result &&
           any (== "    mov dword [rdi + rsi], eax") result
  , testCase "array store IRI8" $
      let sm = Map.fromList [("arr", -8), ("idx", -16), ("val", -21)]
          target = IRTemp "arr" (IRPtr (IRArray IRI8 10))
          index = IRTemp "idx" IRI32
          value = IRTemp "val" IRI8
          result = emitSetElem sm target index value
      in assertBool "should handle byte-sized elements" $
           any (== "    imul rsi, 1") result &&
           any (== "    mov al, byte [rbp-21]") result &&
           any (== "    mov byte [rdi + rsi], al") result
  ]

emitIncDecTests :: TestTree
emitIncDecTests = testGroup "emitIncDec"
  [
    testCase "increment integer" $
      let sm = Map.fromList [("x", -4)]
          result = emitIncDec sm (IRTemp "x" IRI32) "add"
      in assertBool "should add 1" $ any (== "    add dword [rbp-4], 1") result
  , testCase "increment pointer" $
      let sm = Map.fromList [("p", -8)]
          result = emitIncDec sm (IRTemp "p" (IRPtr IRI32)) "add"
      in assertBool "should add size of type" $ any (== "    add qword [rbp-8], 4") result
  , testCase "decrement pointer" $
      let sm = Map.fromList [("p", -8)]
          result = emitIncDec sm (IRTemp "p" (IRPtr IRI64)) "sub"
      in assertBool "should subtract size of type" $ any (== "    sub qword [rbp-8], 8") result
  , testCase "increment non-temp operand" $
      let sm = Map.fromList []
          result = emitIncDec sm (IRGlobal "g" IRI32) "add"
      in assertBool "should emit TODO comment" $ any (== "    ; TODO: IRGlobal \"g\" IRI32 on non-temp/pointer") result
  , testCase "increment IRParam operand" $
      let sm = Map.fromList [("p", -8)]
          result = emitIncDec sm (IRParam "p" (IRPtr IRI32)) "add"
      in assertBool "should add size of inner type" $ any (== "    add qword [rbp-8], 4") result
  ]

emitIncDecHelpersTests :: TestTree
emitIncDecHelpersTests = testGroup "emitIncDecHelper"
  [
    testCase "increment pointer to array" $
      let sm = Map.fromList [("p", -8)]
          result = emitIncDec sm (IRTemp "p" (IRPtr (IRArray IRI32 10))) "add"
      in assertBool "should add size of inner type" $ any (== "    add qword [rbp-8], 4") result
  , testCase "decrement pointer to int" $
      let sm = Map.fromList [("p", -8)]
          result = emitIncDec sm (IRTemp "p" (IRPtr IRI64)) "sub"
      in assertBool "should subtract size of int64" $ any (== "    sub qword [rbp-8], 8") result
  ]

emitAddrTests :: TestTree
emitAddrTests = testGroup "emitAddr"
  [
    testCase "get local address" $
      let sm = Map.fromList [("x", -4), ("px", -12)]
          result = emitAddr sm "px" "x" (IRPtr IRI32)
      in assertBool "should lea local address and store" $ 
           any (== "    lea rax, [rbp-4]") result &&
           any (== "    mov qword [rbp-12], rax") result
  , testCase "get global string address" $
      let sm = Map.fromList [("ps", -8)]
          result = emitAddr sm "ps" "str_1" (IRPtr IRChar)
      in assertBool "should mov global label and store" $
           any (== "    mov rax, str_1") result &&
           any (== "    mov qword [rbp-8], rax") result
  ]

emitConditionalJumpTests :: TestTree
emitConditionalJumpTests = testGroup "emitConditionalJump"
  [
    testCase "jump if zero" $
      let sm = Map.fromList [("cond", -1)]
          result = emitConditionalJump sm (IRTemp "cond" IRBool) "je" "label_target"
      in assertBool "should test and jump" $
           any (== "    test al, al") result &&
           any (== "    je label_target") result
  ]

emitExternsTests :: TestTree
emitExternsTests = testGroup "emitExterns"
  [
    testCase "empty externs" $
      assertBool "should be empty" $ null (emitExterns [])
  , testCase "multiple externs" $
      let result = emitExterns ["printf", "malloc"]
      in assertBool "should format externs" $
           result == ["extern printf", "extern malloc"]
  ]

emitRoDataSectionTests :: TestTree
emitRoDataSectionTests = testGroup "emitRoDataSection"
  [
    testCase "string global" $
      let result = emitRoDataSection [("str1", IRGlobalStringVal "hello")]
      in assertBool "should contain string definition" $
           any (=="str1 db \"hello\", 0") result
  , testCase "float global f32" $
      let result = emitRoDataSection [("f1", IRGlobalFloatVal 3.14 IRF32)]
      in assertBool "should contain float dd" $
           any (=="f1 dd  3.14") result
  ]

emitRmWarningTests :: TestTree
emitRmWarningTests = testGroup "emitRmWarning"
  [
    testCase "check GNU stack note" $
      let result = emitRmWarning
      in assertBool "should have non-exec stack note" $
           any (=="section .note.GNU-stack noalloc noexec nowrite") result
  ]

emitFunctionPrologueTests :: TestTree
emitFunctionPrologueTests = testGroup "emitFunctionPrologue"
  [
    testCase "check standard prologue" $
      let func = IRFunction "test" [] Nothing []
          result = emitFunctionPrologue func 32
      in assertBool "should have prologue instructions" $
           any (=="global test") result &&
           any (=="test:") result &&
           any (=="    push rbp") result &&
           any (=="    sub rsp, 32") result
  ]

emitFunctionEpilogueTests :: TestTree
emitFunctionEpilogueTests = testGroup "emitFunctionEpilogue"
  [
    testCase "check standard epilogue" $
      let result = emitFunctionEpilogue ".L.end"
      in assertBool "should have epilogue instructions" $
           any (==".L.end:") result &&
           any (=="    mov rsp, rbp") result &&
           any (=="    pop rbp") result &&
           any (=="    ret") result
  ]

emitAssignTests :: TestTree
emitAssignTests = testGroup "emitAssign"
  [
    testCase "assign integer literal" $
      let sm = Map.fromList [("x", -8)]
          result = emitAssign sm "x" (IRConstInt 42) IRI32
      in assertBool "should mov literal to stack" $
           any (=="    mov dword [rbp-8], 42") result
  , testCase "assign large integer (requires rax)" $
      let sm = Map.fromList [("x", -8)]
          result = emitAssign sm "x" (IRConstInt 5000000000) IRI64
      in assertBool "should use rax for large literal" $
           any (=="    mov rax, 5000000000") result &&
           any (=="    mov qword [rbp-8], rax") result
  , testCase "assign boolean" $
      let sm = Map.fromList [("b", -1)]
          result = emitAssign sm "b" (IRConstBool True) IRBool
      in assertBool "should mov 1 for true" $
           any (=="    mov byte [rbp-1], 1") result
  ]

emitRetTests :: TestTree
emitRetTests = testGroup "emitRet"
  [
    testCase "return void (Nothing)" $
      let result = emitRet Map.empty ".L.exit" Nothing
      in assertBool "should xor rax" $ any (== "    xor rax, rax") result
  , testCase "return null pointer (IRNull)" $
      let sm = Map.fromList [("p", -8)]
          result = emitRet sm ".L.exit" (Just (IRTemp "p" IRNull))
      in assertBool "should xor rax for null" $ 
           any (== "    xor rax, rax") result && any (== "    jmp .L.exit") result
  , testCase "return float (IRF32)" $
      let sm = Map.fromList [("f", -4)]
          result = emitRet sm ".L.exit" (Just (IRTemp "f" IRF32))
      in assertBool "should load into xmm0" $ 
           any (== "    movss xmm0, dword [rbp-4]") result
  , testCase "return double (IRF64)" $
      let sm = Map.fromList [("d", -8)]
          result = emitRet sm ".L.exit" (Just (IRTemp "d" IRF64))
      in assertBool "should load into xmm0" $ 
           any (== "    movsd xmm0, qword [rbp-8]") result
  , testCase "return integer variable" $
      let sm = Map.fromList [("res", -8)]
          result = emitRet sm ".L.exit" (Just (IRTemp "res" IRI32))
      in assertBool "should load res into eax" $ any (=="    mov eax, dword [rbp-8]") result
  ]

isStaticOperandTests :: TestTree
isStaticOperandTests = testGroup "isStaticOperand"
  [
    testCase "IRConstInt" $
      assertBool "should be static" $
        isStaticOperand (IRConstInt 42) == True
  , testCase "IRConstChar" $
      assertBool "should be static" $
        isStaticOperand (IRConstChar 'A') == True
  , testCase "IRConstBool" $
      assertBool "should be static" $
        isStaticOperand (IRConstBool True) == True
  , testCase "IRConstNull" $
      assertBool "should be static" $
        isStaticOperand IRConstNull == True
  , testCase "IRGlobal" $
      assertBool "should be static" $
        isStaticOperand (IRGlobal "global_var" IRI32) == True
  , testCase "Other operand" $
      assertBool "should not be static" $
        isStaticOperand (IRTemp "temp1" IRI32) == False
  ]

getDataDirectiveTests :: TestTree
getDataDirectiveTests = testGroup "getDataDirective"
  [
    testCase "IRI8" $
      assertBool "should return db" $
        getDataDirective IRI8 == "db"
  , testCase "IRI16" $
      assertBool "should return dw" $
        getDataDirective IRI16 == "dw"
  , testCase "IRI32" $
      assertBool "should return dd" $
        getDataDirective IRI32 == "dd"
  , testCase "IRI64" $
      assertBool "should return dq" $
        getDataDirective IRI64 == "dq"
  , testCase "Array type" $
      assertBool "should return dq for array" $
        getDataDirective (IRArray IRI64 10) == "dq"
  ]

showStaticOperandTests :: TestTree
showStaticOperandTests = testGroup "showStaticOperand"
  [
    testCase "IRConstInt" $
      assertBool "should show integer" $
        showStaticOperand IRI32 (IRConstInt 42) == "42"
  , testCase "IRConstChar" $
      assertBool "should show char as integer" $
        showStaticOperand IRChar (IRConstChar 'A') == "65"
  , testCase "IRConstBool True" $
      assertBool "should show true as 1" $
        showStaticOperand IRBool (IRConstBool True) == "1"
  , testCase "IRConstBool False" $
      assertBool "should show false as 0" $
        showStaticOperand IRBool (IRConstBool False) == "0"
  , testCase "IRConstNull" $
      assertBool "should show null as 0" $
        showStaticOperand IRNull IRConstNull == "0"
  , testCase "IRGlobal" $
      assertBool "should show global name" $
        showStaticOperand IRI32 (IRGlobal "global_var" IRI32) == "global_var"
  , testCase "Other operand" $
      assertBool "should default to 0" $
        showStaticOperand IRI32 (IRTemp "temp1" IRI32) == "0"
  ]

commaSepTests :: TestTree
commaSepTests = testGroup "commaSep"
  [
    testCase "empty list" $
      assertBool "should be empty string" $
        commaSep [] == ""
  , testCase "single element" $
      assertBool "should be single element" $
        commaSep ["a"] == "a"
  , testCase "multiple elements" $
      assertBool "should be comma separated" $
        commaSep ["a", "b", "c"] == "a, b, c"
  ]
