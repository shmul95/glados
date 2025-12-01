module Semantics.VarsSpec (varsSemanticsTests) where

import Rune.AST.Nodes
import Rune.Semantics.Vars (verifVars)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

varsSemanticsTests :: TestTree
varsSemanticsTests =
  testGroup
    "Rune.Semantics.Vars"
    [ expectOk "accepts parameters and local declarations" validProgram,
      expectErr "rejects undefined reference" invalidProgram "missing",
      expectErr "scopes nested block declarations" blockLeakProgram "inner",
      expectOk "validates struct methods recursively" structProgram,
      -- expectErr "struct method reports undefined variable" structMethodErrorProgram "ghost",
      expectErr "override catches undefined variable" overrideLeakProgram "ghost",
      expectOk "override allows parameter usage" overrideValidProgram,
      expectErr "for loop variable does not leak" forLeakProgram "i",
      expectErr "for-each loop variable does not leak" forEachLeakProgram "item",
      expectErr "return expression is validated" returnLeakProgram "retval",
      expectErr "else branch expressions are validated" ifElseElseProgram "elseVar",
      expectOk "struct initialization and access use scoped values" structInitProgram,
      expectOk "binary and unary expressions validate operands" expressionProgram,
      expectErr "call argument must exist" callArgsErrorProgram "arg",
      expectErr "struct init fields must exist" structInitErrorProgram "fieldVal",
      expectErr "access target must exist" accessErrorProgram "target",
      expectErr "binary operands validated" binaryErrorProgram "lhs",
      expectErr "unary operand validated" unaryErrorProgram "value"
    ]

expectOk :: String -> Program -> TestTree
expectOk label program = testCase label $ verifVars program @?= Nothing

expectErr :: String -> Program -> String -> TestTree
expectErr label program missingVar =
  testCase label $ verifVars program @?= Just (undefinedMsg missingVar)

undefinedMsg :: String -> String
undefinedMsg name = "\n\t" ++ name ++ " : var doesn't exist in the scope"

validProgram :: Program
validProgram =
  Program
    "valid"
    [ DefFunction
        "foo"
        [Parameter "arg" TypeI32]
        TypeI32
        [ StmtVarDecl "local" (Just TypeI32) (ExprVar "arg"),
          StmtExpr (ExprCall "bar" [ExprVar "local"]),
          StmtFor "i" Nothing (Just (ExprLitInt 0)) (ExprLitInt 1) [StmtExpr (ExprVar "i")],
          StmtForEach "item" Nothing (ExprVar "local") [StmtExpr (ExprVar "item")],
          StmtReturn Nothing
        ]
    ]

invalidProgram :: Program
invalidProgram =
  Program
    "invalid"
    [ DefFunction
        "foo"
        []
        TypeI32
        [StmtExpr (ExprVar "missing")]
    ]

blockLeakProgram :: Program
blockLeakProgram =
  Program
    "block-leak"
    [ DefFunction
        "foo"
        []
        TypeI32
        [ StmtIf
            (ExprLitBool True)
            [StmtVarDecl "inner" Nothing (ExprLitInt 1)]
            Nothing,
          StmtExpr (ExprVar "inner")
        ]
    ]

structProgram :: Program
structProgram =
  Program
    "struct"
    [ DefStruct
        "Vec"
        []
        [ DefFunction
            "len"
            [Parameter "self" TypeAny]
            TypeI32
            [StmtExpr (ExprVar "self")]
        ]
    ]

-- structMethodErrorProgram :: Program
-- structMethodErrorProgram =
--   Program
--     "struct-error"
--     [ DefStruct
--         "Vec"
--         []
--         [ DefFunction
--             "len"
--             []
--             TypeI32
--             [StmtExpr (ExprVar "ghost")]
--         ]
--     ]

overrideLeakProgram :: Program
overrideLeakProgram =
  Program
    "override-leak"
    [ DefOverride
        "show"
        []
        TypeNull
        [StmtExpr (ExprVar "ghost")]
    ]

overrideValidProgram :: Program
overrideValidProgram =
  Program
    "override-valid"
    [ DefOverride
        "print"
        [Parameter "value" TypeI32]
        TypeNull
        [StmtReturn (Just (ExprVar "value"))]
    ]

forLeakProgram :: Program
forLeakProgram =
  Program
    "for-leak"
    [ DefFunction
        "loop"
        []
        TypeNull
        [ StmtFor "i" Nothing (Just (ExprLitInt 0)) (ExprLitInt 1) [StmtExpr (ExprVar "i")],
          StmtExpr (ExprVar "i")
        ]
    ]

forEachLeakProgram :: Program
forEachLeakProgram =
  Program
    "foreach-leak"
    [ DefFunction
        "loop"
        []
        TypeNull
        [ StmtVarDecl "source" Nothing (ExprLitInt 0),
          StmtForEach "item" Nothing (ExprVar "source") [StmtExpr (ExprVar "item")],
          StmtExpr (ExprVar "item")
        ]
    ]

returnLeakProgram :: Program
returnLeakProgram =
  Program
    "return-leak"
    [ DefFunction
        "returns"
        []
        TypeNull
        [StmtReturn (Just (ExprVar "retval"))]
    ]

ifElseElseProgram :: Program
ifElseElseProgram =
  Program
    "if-else"
    [ DefFunction
        "branch"
        []
        TypeNull
        [ StmtIf
            (ExprLitBool False)
            []
            (Just [StmtExpr (ExprVar "elseVar")])
        ]
    ]

structInitProgram :: Program
structInitProgram =
  Program
    "struct-init"
    [ DefFunction
        "build"
        [Parameter "seed" TypeI32]
        TypeNull
        [ StmtVarDecl "point" Nothing (ExprStructInit "Point" [("x", ExprVar "seed")]),
          StmtExpr (ExprAccess (ExprVar "point") "x")
        ]
    ]

expressionProgram :: Program
expressionProgram =
  Program
    "expr"
    [ DefFunction
        "ops"
        [Parameter "x" TypeI32]
        TypeNull
        [ StmtExpr (ExprBinary Add (ExprVar "x") (ExprUnary Negate (ExprVar "x")))
        ]
    ]

callArgsErrorProgram :: Program
callArgsErrorProgram =
  Program
    "call-args"
    [ DefFunction
        "caller"
        []
        TypeNull
        [StmtExpr (ExprCall "foo" [ExprVar "arg"])]
    ]

structInitErrorProgram :: Program
structInitErrorProgram =
  Program
    "struct-init-err"
    [ DefFunction
        "build"
        []
        TypeNull
        [StmtVarDecl "point" Nothing (ExprStructInit "Point" [("x", ExprVar "fieldVal")])]
    ]

accessErrorProgram :: Program
accessErrorProgram =
  Program
    "access-err"
    [ DefFunction
        "access"
        []
        TypeNull
        [StmtExpr (ExprAccess (ExprVar "target") "field")]
    ]

binaryErrorProgram :: Program
binaryErrorProgram =
  Program
    "binary-err"
    [ DefFunction
        "bin"
        []
        TypeNull
        [StmtExpr (ExprBinary Add (ExprVar "lhs") (ExprLitInt 0))]
    ]

unaryErrorProgram :: Program
unaryErrorProgram =
  Program
    "unary-err"
    [ DefFunction
        "neg"
        []
        TypeNull
        [StmtExpr (ExprUnary Negate (ExprVar "value"))]
    ]
