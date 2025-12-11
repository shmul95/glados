module AST.PrinterSpecs (astPrinterTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual)
import Rune.AST.Nodes
import Rune.AST.Printer (prettyPrint)

--
-- public
--

astPrinterTests :: TestTree
astPrinterTests =
  testGroup
    "AST Printer Tests"
    
    [ programPrinterTests
    , topLevelPrinterTests
    , statementPrinterTests
    , expressionPrinterTests
    ]

--
-- private
--

programPrinterTests :: TestTree
programPrinterTests = testGroup "Program Printer Tests"
  [ testCase "Empty Program" $
      let prog = Program "test.ru" []
          expected = "Program: test.ru"
      in assertEqual "Pretty print empty program" expected (prettyPrint prog)

    , testCase "Program with one function" $
        let func = DefFunction "main" [] TypeI32 [StmtReturn (Just (ExprLitInt 0))]
            prog = Program "main.ru" [func]
            expected = init $ unlines
              [ "Program: main.ru"
              , "  DefFunction main"
              , "    Parameters:"
              , "    ReturnType: i32"
              , "    Body:"
              , "      StmtReturn"
              , "        ExprLitInt 0"
              ]
        in assertEqual "Pretty print program with function" expected (prettyPrint prog)
  ]

topLevelPrinterTests :: TestTree
topLevelPrinterTests = testGroup "TopLevelDef Printer Tests"
  [ testCase "Function with params and body" $
      let params = [Parameter "x" TypeI32, Parameter "y" TypeF64]
          body = [StmtVarDecl "z" (Just TypeF64) (ExprLitFloat 1.5)]
          func = DefFunction "calc" params TypeF64 body
          prog = Program "t.ru" [func]
          expected = init $ unlines
            [ "Program: t.ru"
            , "  DefFunction calc"
            , "    Parameters:"
            , "      x: i32"
            , "      y: f64"
            , "    ReturnType: f64"
            , "    Body:"
            , "      StmtVarDecl z : f64"
            , "      Value:"
            , "        ExprLitFloat 1.5"
            ]
      in assertEqual "Pretty print function" expected (prettyPrint prog)

  , testCase "Struct definition" $
      let fields = [Field "x" TypeI32, Field "y" TypeI32]
          method = DefFunction "init" [] TypeNull []
          st = DefStruct "Vec2" fields [method]
          prog = Program "t.ru" [st]
          expected = init $ unlines
            [ "Program: t.ru"
            , "  DefStruct Vec2"
            , "    Fields:"
            , "      x: i32"
            , "      y: i32"
            , "    Methods:"
            , "      DefFunction init"
            , "        Parameters:"
            , "        ReturnType: null"
            , "        Body:"
            ]
      in assertEqual "Pretty print struct" expected (prettyPrint prog)

  , testCase "Override definition" $
      let ov = DefOverride "toString" [] TypeString []
          prog = Program "t.ru" [ov]
          expected = init $ unlines
            [ "Program: t.ru"
            , "  DefOverride toString"
            , "    Parameters:"
            , "    ReturnType: string"
            , "    Body:"
            ]
      in assertEqual "Pretty print override" expected (prettyPrint prog)
  ]



statementPrinterTests :: TestTree
statementPrinterTests = testGroup "Statement Printer Tests"
  [ testCase "VarDecl with type" $
      let stmt = StmtVarDecl "x" (Just TypeI32) (ExprLitInt 10)
          prog = Program "t.ru" [DefFunction "f" [] TypeNull [stmt]]
          expected = init $ unlines
            [ "Program: t.ru"
            , "  DefFunction f"
            , "    Parameters:"
            , "    ReturnType: null"
            , "    Body:"
            , "      StmtVarDecl x : i32"
            , "      Value:"
            , "        ExprLitInt 10"
            ]
      in assertEqual "VarDecl with type" expected (prettyPrint prog)

  , testCase "Assignment" $
      let stmt = StmtAssignment (ExprVar "x") (ExprLitInt 5)
          prog = Program "t.ru" [DefFunction "f" [] TypeNull [stmt]]
          expected = init $ unlines
            [ "Program: t.ru"
            , "  DefFunction f"
            , "    Parameters:"
            , "    ReturnType: null"
            , "    Body:"
            , "      StmtAssignment"
            , "        LValue:"
            , "          ExprVar x"
            , "        RValue:"
            , "          ExprLitInt 5"
            ]
      in assertEqual "Assignment" expected (prettyPrint prog)

  , testCase "If Else" $
      let stmt = StmtIf (ExprLitBool True) [StmtStop] (Just [StmtNext])
          prog = Program "t.ru" [DefFunction "f" [] TypeNull [stmt]]
          expected = init $ unlines
            [ "Program: t.ru"
            , "  DefFunction f"
            , "    Parameters:"
            , "    ReturnType: null"
            , "    Body:"
            , "      StmtIf"
            , "        Condition:"
            , "          ExprLitBool True"
            , "        Then:"
            , "          StmtStop"
            , "        Else:"
            , "          StmtNext"
            ]
      in assertEqual "If Else" expected (prettyPrint prog)

  , testCase "For Loop" $
      let stmt = StmtFor "i" (Just TypeI32) (Just (ExprLitInt 0)) (ExprLitInt 10) [StmtNext]
          prog = Program "t.ru" [DefFunction "f" [] TypeNull [stmt]]
          expected = init $ unlines
            [ "Program: t.ru"
            , "  DefFunction f"
            , "    Parameters:"
            , "    ReturnType: null"
            , "    Body:"
            , "      StmtFor i : i32"
            , "        Start:"
            , "          ExprLitInt 0"
            , "        End:"
            , "          ExprLitInt 10"
            , "        Body:"
            , "          StmtNext"
            ]
      in assertEqual "For Loop" expected (prettyPrint prog)

  , testCase "For Loop implicit start" $
      let stmt = StmtFor "i" Nothing Nothing (ExprLitInt 10) []
          prog = Program "t.ru" [DefFunction "f" [] TypeNull [stmt]]
          expected = init $ unlines
            [ "Program: t.ru"
            , "  DefFunction f"
            , "    Parameters:"
            , "    ReturnType: null"
            , "    Body:"
            , "      StmtFor i"
            , "        Start: <Implicit>"
            , "        End:"
            , "          ExprLitInt 10"
            , "        Body:"
            ]
      in assertEqual "For Loop implicit start" expected (prettyPrint prog)

  , testCase "ForEach" $
      let stmt = StmtForEach "item" (Just TypeString) (ExprVar "items") []
          prog = Program "t.ru" [DefFunction "f" [] TypeNull [stmt]]
          expected = init $ unlines
            [ "Program: t.ru"
            , "  DefFunction f"
            , "    Parameters:"
            , "    ReturnType: null"
            , "    Body:"
            , "      StmtForEach item : string"
            , "        Iterable:"
            , "          ExprVar items"
            , "        Body:"
            ]
      in assertEqual "ForEach" expected (prettyPrint prog)

  , testCase "Loop" $
      let stmt = StmtLoop [StmtStop]
          prog = Program "t.ru" [DefFunction "f" [] TypeNull [stmt]]
          expected = init $ unlines
            [ "Program: t.ru"
            , "  DefFunction f"
            , "    Parameters:"
            , "    ReturnType: null"
            , "    Body:"
            , "      StmtLoop"
            , "        Body:"
            , "          StmtStop"
            ]
      in assertEqual "Loop" expected (prettyPrint prog)

  , testCase "Expr Statement" $
      let stmt = StmtExpr (ExprCall "f" [])
          prog = Program "t.ru" [DefFunction "f" [] TypeNull [stmt]]
          expected = init $ unlines
            [ "Program: t.ru"
            , "  DefFunction f"
            , "    Parameters:"
            , "    ReturnType: null"
            , "    Body:"
            , "      StmtExpr"
            , "        ExprCall f"
            , "        Arguments:"
            ]
      in assertEqual "Expr Statement" expected (prettyPrint prog)
  ]





expressionPrinterTests :: TestTree
expressionPrinterTests = testGroup "Expression Printer Tests"
  [ testCase "Binary Op" $
      let expr = ExprBinary Add (ExprLitInt 1) (ExprLitInt 2)
          stmt = StmtReturn (Just expr)
          prog = Program "t.ru" [DefFunction "f" [] TypeNull [stmt]]
          expected = init $ unlines
            [ "Program: t.ru"
            , "  DefFunction f"
            , "    Parameters:"
            , "    ReturnType: null"
            , "    Body:"
            , "      StmtReturn"
            , "        ExprBinary +"
            , "          ExprLitInt 1"
            , "          ExprLitInt 2"
            ]
      in assertEqual "Binary Op" expected (prettyPrint prog)

  , testCase "Unary Op" $
      let expr = ExprUnary Negate (ExprLitInt 1)
          stmt = StmtReturn (Just expr)
          prog = Program "t.ru" [DefFunction "f" [] TypeNull [stmt]]
          expected = init $ unlines
            [ "Program: t.ru"
            , "  DefFunction f"
            , "    Parameters:"
            , "    ReturnType: null"
            , "    Body:"
            , "      StmtReturn"
            , "        ExprUnary -"
            , "          ExprLitInt 1"
            ]
      in assertEqual "Unary Op" expected (prettyPrint prog)

  , testCase "Call" $
      let expr = ExprCall "add" [ExprLitInt 1, ExprLitInt 2]
          stmt = StmtReturn (Just expr)
          prog = Program "t.ru" [DefFunction "f" [] TypeNull [stmt]]
          expected = init $ unlines
            [ "Program: t.ru"
            , "  DefFunction f"
            , "    Parameters:"
            , "    ReturnType: null"
            , "    Body:"
            , "      StmtReturn"
            , "        ExprCall add"
            , "        Arguments:"
            , "          ExprLitInt 1"
            , "          ExprLitInt 2"
            ]
      in assertEqual "Call" expected (prettyPrint prog)

  , testCase "Struct Init" $
      let expr = ExprStructInit "Point" [("x", ExprLitInt 1), ("y", ExprLitInt 2)]
          stmt = StmtReturn (Just expr)
          prog = Program "t.ru" [DefFunction "f" [] TypeNull [stmt]]
          expected = init $ unlines
            [ "Program: t.ru"
            , "  DefFunction f"
            , "    Parameters:"
            , "    ReturnType: null"
            , "    Body:"
            , "      StmtReturn"
            , "        ExprStructInit Point"
            , "        Fields:"
            , "          x:"
            , "          "
            , "            ExprLitInt 1"
            , "          y:"
            , "          "
            , "            ExprLitInt 2"
            ]
      in assertEqual "Struct Init" expected (prettyPrint prog)

  , testCase "Access" $
      let expr = ExprAccess (ExprVar "p") "x"
          stmt = StmtReturn (Just expr)
          prog = Program "t.ru" [DefFunction "f" [] TypeNull [stmt]]
          expected = init $ unlines
            [ "Program: t.ru"
            , "  DefFunction f"
            , "    Parameters:"
            , "    ReturnType: null"
            , "    Body:"
            , "      StmtReturn"
            , "        ExprAccess .x"
            , "          ExprVar p"
            ]
      in assertEqual "Access" expected (prettyPrint prog)

  , testCase "Literals" $
      let exprs = [ ExprLitInt 1
                  , ExprLitFloat 1.5
                  , ExprLitString "s"
                  , ExprLitChar 'c'
                  , ExprLitBool True
                  , ExprLitNull
                  ]
          stmts = map (StmtExpr) exprs
          prog = Program "t.ru" [DefFunction "f" [] TypeNull stmts]
          expected = init $ unlines
            [ "Program: t.ru"
            , "  DefFunction f"
            , "    Parameters:"
            , "    ReturnType: null"
            , "    Body:"
            , "      StmtExpr"
            , "        ExprLitInt 1"
            , "      StmtExpr"
            , "        ExprLitFloat 1.5"
            , "      StmtExpr"
            , "        ExprLitString \"s\""
            , "      StmtExpr"
            , "        ExprLitChar 'c'"
            , "      StmtExpr"
            , "        ExprLitBool True"
            , "      StmtExpr"
            , "        ExprLitNull"
            ]
      in assertEqual "Literals" expected (prettyPrint prog)
  ]






