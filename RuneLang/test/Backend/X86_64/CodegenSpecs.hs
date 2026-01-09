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
    emitDataSectionTests,
    emitTextSectionTests,
    emitRmWarningTests,
    emitFunctionTests,
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
    setupCallArgsTests,
    emitInstructionTests,
    emitAssemblyTests
  ]

--
-- private
--

emitAssemblyTests :: TestTree
emitAssemblyTests = testGroup "emitAssembly"
  [ testCase "generate complete assembly from IRProgram" $
      let program = IRProgram "complete_program"
            [ IRExtern "malloc"
            , IRGlobalDef "glob_msg" (IRGlobalStringVal "hello")
            , IRFunctionDef (IRFunction "main_fn" [] Nothing 
                [ IRALLOC_ARRAY "static_arr" IRI32 [IRConstInt 42]
                , IRRET Nothing
                ]
                False)
            ]
          resultLines = lines (emitAssembly program)
      in assertBool "should contain all assembly sections and labels" $
           any (== "extern malloc") resultLines &&
           any (== "section .rodata") resultLines &&
           any (== "glob_msg db \"hello\", 0") resultLines &&
           any (== "section .data") resultLines &&
           any (== "main_fn_static_arr_lit: dd 42, 0") resultLines &&
           any (== "section .text") resultLines &&
           any (== "main_fn:") resultLines &&
           any (== "section .note.GNU-stack noalloc noexec nowrite") resultLines
  ]

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
          func = IRFunction "main" [] Nothing [instr] False
          result = collectStaticArrays [func]
      in case result of
           [(n, t, v)] -> assertBool "content match" $ n == "main_arr_lit" && t == IRI32 && v == values
           _           -> assertBool "expected exactly one static array" False
  , testCase "ignore dynamic array" $
      let instr = IRALLOC_ARRAY "arr" IRI32 [IRTemp "t" IRI32]
          func = IRFunction "main" [] Nothing [instr] False
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
  , testCase "mix i64 and f64" $
      let params = [("x", IRI64), ("y", IRF64), ("z", IRI32)]
          sm = Map.fromList [("x", -8), ("y", -16), ("z", -20)]
          result = emitParameters params sm
      in assertBool "should map to correct registers" $
           any (== "    mov qword [rbp-8], rdi") result &&
           any (== "    movsd qword [rbp-16], xmm0") result &&
           any (== "    mov dword [rbp-20], esi") result
  ]


emitCallTests :: TestTree
emitCallTests = testGroup "emitCall"
  [
    testCase "standard function call (non-printf)" $
      let sm = Map.fromList [("arg", -4), ("res", -8)]
          args = [IRTemp "arg" IRI32]
          result = emitCall sm "res" "custom_func" args (Just IRI32)
      in assertBool "should setup args and call without printf fixup" $
           any (== "    movsxd rdi, dword [rbp-4]") result &&
           any (== "    call custom_func") result &&
           any (== "    mov dword [rbp-8], eax") result &&
           not (any (== "    mov eax, 1") result)
  , testCase "printf with IRF32 argument" $
      let sm = Map.fromList [("f", -4)]
          args = [IRTemp "f" IRF32]
          result = emitCall sm "" "printf" args Nothing
      in assertBool "should convert float to double and set eax to 1" $
           any (== "    cvtss2sd xmm0, xmm0") result &&
           any (== "    mov eax, 1") result
  , testCase "printf with IRF64 argument" $
      let sm = Map.fromList [("d", -8)]
          args = [IRTemp "d" IRF64]
          result = emitCall sm "" "printf" args Nothing
      in assertBool "should set eax to 1 without conversion" $
           any (== "    mov eax, 1") result &&
           not (any (== "    cvtss2sd xmm0, xmm0") result)
  , testCase "printf with no float arguments" $
      let sm = Map.fromList [("i", -4)]
          args = [IRTemp "i" IRI32]
          result = emitCall sm "" "printf" args Nothing
      in assertBool "should set eax to 0 (xor eax, eax)" $
           any (== "    xor eax, eax") result
  , testCase "printf with mixed args (int first)" $
      let sm = Map.fromList [("i", -4), ("f", -8), ("res", -12)]
          args = [IRTemp "i" IRI32, IRTemp "f" IRF64]
          result = emitCall sm "res" "printf" args (Just IRI32)
      in assertBool "should find float arg and save result to valid stack slot" $
           any (== "    mov eax, 1") result &&
           any (== "    mov dword [rbp-12], eax") result
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
  [ testCase "empty globals" $
      assertBool "should be empty" $ null (emitRoDataSection [])
  , testCase "all global types" $
      let gs = [ ("s1", IRGlobalStringVal "hi")
               , ("f1", IRGlobalFloatVal 1.1 IRF32)
               , ("f2", IRGlobalFloatVal 2.2 IRF64)
               , ("f3", IRGlobalFloatVal 3.3 IRI32)
               ]
          result = emitRoDataSection gs
      in assertBool "should format section and all types" $
           take 1 result == ["section .rodata"] &&
           any (== "s1 db \"hi\", 0") result &&
           any (== "f1 dd  1.1") result &&
           any (== "f2 dq  2.2") result &&
           any (== "f3 dd  3.3") result
  ]

emitDataSectionTests :: TestTree
emitDataSectionTests = testGroup "emitDataSection"
  [ testCase "no static arrays" $
      let fs = [IRFunction "f" [] Nothing [] False]
      in assertBool "should be empty" $ null (emitDataSection fs)
  , testCase "with static array" $
      let fs = [IRFunction "m" [] Nothing [IRALLOC_ARRAY "a" IRI32 [IRConstInt 1, IRConstInt 2]] False]
          result = emitDataSection fs
      in assertBool "should format section and array definition" $
           take 1 result == ["section .data"] &&
           any (== "m_a_lit: dd 1, 2, 0") result
  ]

emitTextSectionTests :: TestTree
emitTextSectionTests = testGroup "emitTextSection"
  [ testCase "empty functions" $
      assertBool "should be empty" $ null (emitTextSection [])
  , testCase "with functions" $
      let fs = [IRFunction "f1" [] Nothing [] False, IRFunction "f2" [] Nothing [] False]
          result = emitTextSection fs
      in assertBool "should format section and functions" $
           take 1 result == ["section .text"] &&
           any (== "f1:") result &&
           any (== "f2:") result
  ]

emitRmWarningTests :: TestTree
emitRmWarningTests = testGroup "emitRmWarning"
  [
    testCase "check GNU stack note" $
      let result = emitRmWarning
      in assertBool "should have non-exec stack note" $
           any (=="section .note.GNU-stack noalloc noexec nowrite") result
  ]

emitFunctionTests :: TestTree
emitFunctionTests = testGroup "emitFunction"
  [
    testCase "full function emission" $
      let body = [IRRET (Just (IRTemp "arg1" IRI32))]
          params = [("arg1", IRI32)]
          fn = IRFunction "test_func" params Nothing body True
          result = emitFunction fn
      in assertBool "should contain all function components" $
           any (== "global test_func:function") result &&
           any (== "test_func:") result &&
           any (== "    push rbp") result &&
           -- arg1 (4 bytes) -> stack size 16 (aligned)
           any (== "    sub rsp, 16") result &&
           any (== "    mov dword [rbp-4], edi") result &&
           any (== "    mov eax, dword [rbp-4]") result &&
           any (== ".L.function_end_test_func:") result &&
           any (== "    ret") result

  , testCase "empty function body" $
      let fn = IRFunction "empty" [] Nothing [] False
          result = emitFunction fn
      in assertBool "should still emit prologue and epilogue" $
           any (== "empty:") result &&
           any (== "    push rbp") result &&
           any (== ".L.function_end_empty:") result &&
           any (== "    ret") result
  ]

emitFunctionPrologueTests :: TestTree
emitFunctionPrologueTests = testGroup "emitFunctionPrologue"
  [
    testCase "check standard prologue" $
      let func = IRFunction "test" [] Nothing [] True
          result = emitFunctionPrologue func 32
      in assertBool "should have prologue instructions" $
           any (=="global test:function") result &&
           any (=="test:") result &&
           any (=="    push rbp") result &&
           any (=="    sub rsp, 32") result

  , testCase "check prologue without stack allocation" $
      let func = IRFunction "test" [] Nothing [] True
          result = emitFunctionPrologue func 0
      in assertBool "should not have sub rsp" $
           any (=="test:") result &&
           any (=="    push rbp") result &&
           not (any (== "    sub rsp, 0") result)
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
    testCase "IRConstInt small" $
      let sm = Map.fromList [("dest", -4)]
          result = emitAssign sm "dest" (IRConstInt 42) IRI32
      in assertBool "should mov direct to stack" $
           any (== "    mov dword [rbp-4], 42") result
  , testCase "IRConstInt large (needs rax)" $
      let sm = Map.fromList [("dest", -8)]
          result = emitAssign sm "dest" (IRConstInt 5000000000) IRI64
      in assertBool "should load rax then store" $
           any (== "    mov rax, 5000000000") result &&
           any (== "    mov qword [rbp-8], rax") result
  , testCase "IRGlobal IRF32 to IRF32" $
      let sm = Map.fromList [("dest", -4)]
          result = emitAssign sm "dest" (IRGlobal "f32_glob" IRF32) IRF32
      in assertBool "should use movss" $
           any (== "    movss xmm0, dword [rel f32_glob]") result &&
           any (== "    movss dword [rbp-4], xmm0") result
  , testCase "IRGlobal IRF32 to IRF64" $
      let sm = Map.fromList [("dest", -8)]
          result = emitAssign sm "dest" (IRGlobal "f32_glob" IRF32) IRF64
      in assertBool "should use cvtss2sd" $
           any (== "    movss xmm0, dword [rel f32_glob]") result &&
           any (== "    cvtss2sd xmm0, xmm0") result &&
           any (== "    movsd qword [rbp-8], xmm0") result
  , testCase "IRGlobal IRF64 to IRF64" $
      let sm = Map.fromList [("dest", -8)]
          result = emitAssign sm "dest" (IRGlobal "f64_glob" IRF64) IRF64
      in assertBool "should use movsd" $
           any (== "    movsd xmm0, qword [rel f64_glob]") result &&
           any (== "    movsd qword [rbp-8], xmm0") result
  , testCase "IRGlobal IRF64 to IRF32" $
      let sm = Map.fromList [("dest", -4)]
          result = emitAssign sm "dest" (IRGlobal "f64_glob" IRF64) IRF32
      in assertBool "should use cvtsd2ss" $
           any (== "    movsd xmm0, qword [rel f64_glob]") result &&
           any (== "    cvtsd2ss xmm0, xmm0") result &&
           any (== "    movss dword [rbp-4], xmm0") result
  , testCase "IRConstChar" $
      let sm = Map.fromList [("dest", -1)]
          result = emitAssign sm "dest" (IRConstChar 'A') IRChar
      in assertBool "should mov byte 65" $
           any (== "    mov byte [rbp-1], 65") result
  , testCase "IRConstBool" $
      let sm = Map.fromList [("dest", -1)]
          result = emitAssign sm "dest" (IRConstBool True) IRBool
      in assertBool "should mov byte 1" $
           any (== "    mov byte [rbp-1], 1") result
  , testCase "IRConstNull" $
      let sm = Map.fromList [("dest", -8)]
          result = emitAssign sm "dest" IRConstNull (IRPtr IRI32)
      in assertBool "should mov qword 0" $
           any (== "    mov qword [rbp-8], 0") result
  , testCase "IRGlobal generic" $
      let sm = Map.fromList [("dest", -8)]
          result = emitAssign sm "dest" (IRGlobal "my_glob" IRI64) IRI64
      in assertBool "should load label address into rax" $
           any (== "    mov rax, my_glob") result &&
           any (== "    mov qword [rbp-8], rax") result
  , testCase "IRTemp (stack to stack)" $
      let sm = Map.fromList [("dest", -8), ("tmp", -16)]
          result = emitAssign sm "dest" (IRTemp "tmp" IRI64) IRI64
      in assertBool "should move via rax" $
           any (== "    mov rax, qword [rbp-16]") result &&
           any (== "    mov qword [rbp-8], rax") result
  , testCase "IRParam (stack to stack)" $
      let sm = Map.fromList [("dest", -4), ("p1", -8)]
          result = emitAssign sm "dest" (IRParam "p1" IRI32) IRI32
      in assertBool "should move via eax" $
           any (== "    mov eax, dword [rbp-8]") result &&
           any (== "    mov dword [rbp-4], eax") result
  , testCase "Unsupported operand" $
      let sm = Map.fromList [("dest", -8)]
          result = emitAssign sm "dest" (IRConstFloat 1.0) IRI64
      in assertBool "should emit warning and zero out" $
           any (== "    ; WARNING: Unsupported IRASSIGN operand: IRConstFloat 1.0") result &&
           any (== "    mov qword [rbp-8], 0") result
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

emitInstructionTests :: TestTree
emitInstructionTests = testGroup "emitInstruction"
  [
    testCase "IRASSIGN" $
      let sm = Map.fromList [("x", -4)]
          instr = IRASSIGN "x" (IRConstInt 42) IRI32
          result = emitInstruction sm "" "" instr
      in assertBool "should call emitAssign" $ any (== "    mov dword [rbp-4], 42") result
  , testCase "IRLABEL" $
      let instr = IRLABEL (IRLabel "L1")
          result = emitInstruction Map.empty "" "" instr
      in assertBool "should emit label" $ result == ["L1:"]
  , testCase "IRJUMP" $
      let instr = IRJUMP (IRLabel "L1")
          result = emitInstruction Map.empty "" "" instr
      in assertBool "should emit jmp" $ result == ["    jmp L1"]
  , testCase "IRJUMP_EQ0" $
      let sm = Map.fromList [("c", -1)]
          instr = IRJUMP_EQ0 (IRTemp "c" IRBool) (IRLabel "L1")
          result = emitInstruction sm "" "" instr
      in assertBool "should emit je" $ any (== "    je L1") result
  , testCase "IRJUMP_FALSE" $
      let sm = Map.fromList [("c", -1)]
          instr = IRJUMP_FALSE (IRTemp "c" IRBool) (IRLabel "L1")
          result = emitInstruction sm "" "" instr
      in assertBool "should emit je" $ any (== "    je L1") result
  , testCase "IRJUMP_TRUE" $
      let sm = Map.fromList [("c", -1)]
          instr = IRJUMP_TRUE (IRTemp "c" IRBool) (IRLabel "L1")
          result = emitInstruction sm "" "" instr
      in assertBool "should emit jne" $ any (== "    jne L1") result
  , testCase "IRCALL" $
      let sm = Map.fromList [("r", -8)]
          instr = IRCALL "r" "my_func" [] (Just IRI64)
          result = emitInstruction sm "" "" instr
      in assertBool "should call emitCall" $ any (== "    call my_func") result
  , testCase "IRRET" $
      let instr = IRRET Nothing
          result = emitInstruction Map.empty "end" "" instr
      in assertBool "should call emitRet" $ any (== "    jmp end") result
  , testCase "IRDEREF" $
      let sm = Map.fromList [("p", -8), ("d", -4)]
          instr = IRDEREF "d" (IRTemp "p" (IRPtr IRI32)) IRI32
          result = emitInstruction sm "" "" instr
      in assertBool "should call emitDeref" $ any (== "    mov eax, dword [rax]") result
  , testCase "IRALLOC_ARRAY" $
      let sm = Map.fromList [("a", -8)]
          instr = IRALLOC_ARRAY "a" IRI32 [IRConstInt 1]
          result = emitInstruction sm "" "f" instr
      in assertBool "should call emitAllocArray" $ any (== "    mov rax, f_a_lit") result
  , testCase "IRGET_ELEM" $
      let sm = Map.fromList [("a", -8), ("i", -4), ("d", -4)]
          instr = IRGET_ELEM "d" (IRTemp "a" (IRPtr (IRArray IRI32 1))) (IRTemp "i" IRI32) IRI32
          result = emitInstruction sm "" "" instr
      in assertBool "should call emitGetElem" $ any (== "    imul rsi, 4") result
  , testCase "IRSET_ELEM" $
      let sm = Map.fromList [("a", -8), ("i", -4), ("v", -4)]
          instr = IRSET_ELEM (IRTemp "a" (IRPtr (IRArray IRI32 1))) (IRTemp "i" IRI32) (IRTemp "v" IRI32)
          result = emitInstruction sm "" "" instr
      in assertBool "should call emitSetElem" $ any (== "    mov dword [rdi + rsi], eax") result
  , testCase "IRINC" $
      let sm = Map.fromList [("x", -4)]
          instr = IRINC (IRTemp "x" IRI32)
          result = emitInstruction sm "" "" instr
      in assertBool "should call emitIncDec add" $ any (== "    add dword [rbp-4], 1") result
  , testCase "IRDEC" $
      let sm = Map.fromList [("x", -4)]
          instr = IRDEC (IRTemp "x" IRI32)
          result = emitInstruction sm "" "" instr
      in assertBool "should call emitIncDec sub" $ any (== "    sub dword [rbp-4], 1") result
  , testCase "IRADDR" $
      let sm = Map.fromList [("x", -4), ("p", -8)]
          instr = IRADDR "p" "x" (IRPtr IRI32)
          result = emitInstruction sm "" "" instr
      in assertBool "should call emitAddr" $ any (== "    lea rax, [rbp-4]") result
  , testCase "Binary Operators" $
      let sm = Map.fromList [("l", -4), ("r", -4), ("d", -4)]
          l = IRTemp "l" IRI32
          r = IRTemp "r" IRI32
          cases = [ (IRADD_OP "d" l r IRI32, "add")
                  , (IRSUB_OP "d" l r IRI32, "sub")
                  , (IRMUL_OP "d" l r IRI32, "imul")
                  , (IRAND_OP "d" l r IRI32, "and")
                  , (IROR_OP "d" l r IRI32, "or")
                  ]
      in mapM_ (\(instr, op) -> assertBool op $ any (== "    " <> op <> " eax, ebx") (emitInstruction sm "" "" instr)) cases
  , testCase "IRDIV_OP / IRMOD_OP" $
      let sm = Map.fromList [("l", -8), ("r", -8), ("d", -8)]
          l = IRTemp "l" IRI64
          r = IRTemp "r" IRI64
          resultDiv = emitInstruction sm "" "" (IRDIV_OP "d" l r IRI64)
          resultMod = emitInstruction sm "" "" (IRMOD_OP "d" l r IRI64)
      in assertBool "div" (any (== "    idiv rbx") resultDiv) >> assertBool "mod" (any (== "    idiv rbx") resultMod)
  , testCase "Comparison Operators" $
      let sm = Map.fromList [("l", -4), ("r", -4), ("d", -1)]
          l = IRTemp "l" IRI32
          r = IRTemp "r" IRI32
          cases = [ (IRCMP_EQ "d" l r, "sete")
                  , (IRCMP_NEQ "d" l r, "setne")
                  , (IRCMP_LT "d" l r, "setl")
                  , (IRCMP_LTE "d" l r, "setle")
                  , (IRCMP_GT "d" l r, "setg")
                  , (IRCMP_GTE "d" l r, "setge")
                  ]
      in mapM_ (\(instr, op) -> assertBool op $ any (== "    " <> op <> " al") (emitInstruction sm "" "" instr)) cases
  ]
