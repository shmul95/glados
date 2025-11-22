{-# LANGUAGE OverloadedStrings #-}

module ASTSpec (spec) where

import Test.Hspec
import AST
import SExpr

spec :: Spec
spec = do
  describe "Ast data constructors" $ do
    it "creates all Ast types" $ do
      let define = Define (AstSymbol "x") (AstInteger 5)
      let call = Call "+" [AstInteger 1, AstInteger 2]
      let lambda = Lambda [AstSymbol "x"] (AstInteger 1)
      let ifExpr = If (AstBoolean True) (AstInteger 1) (AstInteger 0)

      define `shouldBe` Define (AstSymbol "x") (AstInteger 5)
      call `shouldBe` Call "+" [AstInteger 1, AstInteger 2]
      lambda `shouldBe` Lambda [AstSymbol "x"] (AstInteger 1)
      ifExpr `shouldBe` If (AstBoolean True) (AstInteger 1) (AstInteger 0)

  describe "sexprToAST" $ do
    it "converts integers" $ do
      sexprToAST (Integer 42) `shouldBe` Just (AstInteger 42)
      sexprToAST (Integer 0) `shouldBe` Just (AstInteger 0)
      sexprToAST (Integer (-17)) `shouldBe` Just (AstInteger (-17))

    it "converts symbols" $ do
      sexprToAST (Symbol "x") `shouldBe` Just (AstSymbol "x")
      sexprToAST (Symbol "hello") `shouldBe` Just (AstSymbol "hello")
      sexprToAST (Symbol "+") `shouldBe` Just (AstSymbol "+")

    it "converts define expressions" $ do
      let sexpr = List [Symbol "define", Symbol "x", Integer 5]
      sexprToAST sexpr `shouldBe` Just (Define (AstSymbol "x") (AstInteger 5))

    it "rejects malformed define expressions" $ do
      sexprToAST (List [Symbol "define"]) `shouldBe` Nothing
      sexprToAST (List [Symbol "define", Symbol "x"]) `shouldBe` Nothing
      sexprToAST (List [Symbol "define", Symbol "x", Integer 1, Integer 2]) `shouldBe` Nothing

    it "converts lambda expressions" $ do
      let sexpr = List [Symbol "lambda", List [Symbol "x"], Integer 1]
      sexprToAST sexpr `shouldBe` Just (Lambda [AstSymbol "x"] (AstInteger 1))

      let sexpr2 = List [Symbol "lambda", List [Symbol "x", Symbol "y"],
                        List [Symbol "+", Symbol "x", Symbol "y"]]
      sexprToAST sexpr2 `shouldBe`
        Just (Lambda [AstSymbol "x", AstSymbol "y"]
                    (Call "+" [AstSymbol "x", AstSymbol "y"]))

    it "rejects malformed lambda expressions" $ do
      sexprToAST (List [Symbol "lambda"]) `shouldBe` Nothing
      sexprToAST (List [Symbol "lambda", Symbol "x"]) `shouldBe` Nothing
      sexprToAST (List [Symbol "lambda", List [Symbol "x"]]) `shouldBe` Nothing

    it "converts if expressions" $ do
      let sexpr = List [Symbol "if", Symbol "#t", Integer 1, Integer 0]
      sexprToAST sexpr `shouldBe`
        Just (If (AstSymbol "#t") (AstInteger 1) (AstInteger 0))

    it "converts function calls" $ do
      let sexpr = List [Symbol "+", Integer 1, Integer 2]
      sexprToAST sexpr `shouldBe` Just (Call "+" [AstInteger 1, AstInteger 2])

      let sexpr2 = List [Symbol "eq?", Symbol "x", Integer 5]
      sexprToAST sexpr2 `shouldBe` Just (Call "eq?" [AstSymbol "x", AstInteger 5])

    it "converts non-function lists to AstList" $ do
      let sexpr = List [Symbol "unknown", Integer 1]
      sexprToAST sexpr `shouldBe` Just (AstList [AstSymbol "unknown", AstInteger 1])

    it "converts empty lists" $ do
      sexprToAST (List []) `shouldBe` Just (AstList [])

    it "converts regular lists" $ do
      let sexpr = List [Integer 1, Integer 2, Integer 3]
      sexprToAST sexpr `shouldBe` Just (AstList [AstInteger 1, AstInteger 2, AstInteger 3])

  describe "extractString" $ do
    it "extracts string from AstSymbol" $ do
      extractString (AstSymbol "test") `shouldBe` Just "test"
      extractString (AstSymbol "") `shouldBe` Just ""

    it "returns Nothing for non-symbols" $ do
      extractString (AstInteger 42) `shouldBe` Nothing
      extractString (AstBoolean True) `shouldBe` Nothing

  describe "compEnv" $ do
    it "finds variables in environment" $ do
      let env = [(AstSymbol "x", AstInteger 5), (AstSymbol "y", AstInteger 10)]
      compEnv env "x" `shouldBe` Just (AstInteger 5)
      compEnv env "y" `shouldBe` Just (AstInteger 10)

    it "returns Nothing for non-existent variables" $ do
      let env = [(AstSymbol "x", AstInteger 5)]
      compEnv env "z" `shouldBe` Nothing

    it "returns Nothing for empty environment" $ do
      compEnv [] "x" `shouldBe` Nothing

  describe "extractInteger" $ do
    it "extracts integers from evaluated expressions" $ do
      let env = []
      extractInteger env (AstInteger 42) `shouldBe` Just 42
      extractInteger env (AstInteger 0) `shouldBe` Just 0

    it "extracts integers from variables in environment" $ do
      let env = [(AstSymbol "x", AstInteger 5)]
      extractInteger env (AstSymbol "x") `shouldBe` Just 5

    it "returns Nothing for non-integer values" $ do
      let env = []
      extractInteger env (AstBoolean True) `shouldBe` Nothing
      extractInteger env (AstSymbol "undefined") `shouldBe` Nothing

  describe "handleString" $ do
    it "handles boolean literals" $ do
      let env = []
      handleString env "#t" `shouldBe` Just (AstBoolean True)
      handleString env "#f" `shouldBe` Just (AstBoolean False)

    it "looks up variables in environment" $ do
      let env = [(AstSymbol "x", AstInteger 5)]
      handleString env "x" `shouldBe` Just (AstInteger 5)

    it "returns symbol for undefined variables" $ do
      let env = []
      handleString env "undefined" `shouldBe` Just (AstSymbol "undefined")

  describe "handleCall" $ do
    it "performs addition" $ do
      let env = []
      handleCall env "+" [AstInteger 3, AstInteger 2] `shouldBe` Just (AstInteger 5)
      handleCall env "+" [AstInteger 0, AstInteger 0] `shouldBe` Just (AstInteger 0)
      handleCall env "+" [AstInteger (-1), AstInteger 1] `shouldBe` Just (AstInteger 0)

    it "performs subtraction" $ do
      let env = []
      handleCall env "-" [AstInteger 5, AstInteger 3] `shouldBe` Just (AstInteger 2)
      handleCall env "-" [AstInteger 0, AstInteger 5] `shouldBe` Just (AstInteger (-5))

    it "performs multiplication" $ do
      let env = []
      handleCall env "*" [AstInteger 3, AstInteger 4] `shouldBe` Just (AstInteger 12)
      handleCall env "*" [AstInteger 0, AstInteger 5] `shouldBe` Just (AstInteger 0)

    it "performs division" $ do
      let env = []
      handleCall env "div" [AstInteger 10, AstInteger 3] `shouldBe` Just (AstInteger 3)
      handleCall env "div" [AstInteger 6, AstInteger 2] `shouldBe` Just (AstInteger 3)

    it "handles division by zero" $ do
      let env = []
      handleCall env "div" [AstInteger 10, AstInteger 0] `shouldBe` Nothing

    it "performs modulo" $ do
      let env = []
      handleCall env "mod" [AstInteger 10, AstInteger 3] `shouldBe` Just (AstInteger 1)
      handleCall env "mod" [AstInteger 6, AstInteger 2] `shouldBe` Just (AstInteger 0)

    it "handles modulo by zero" $ do
      let env = []
      handleCall env "mod" [AstInteger 10, AstInteger 0] `shouldBe` Nothing

    it "performs equality comparison" $ do
      let env = []
      handleCall env "eq?" [AstInteger 5, AstInteger 5] `shouldBe` Just (AstBoolean True)
      handleCall env "eq?" [AstInteger 3, AstInteger 5] `shouldBe` Just (AstBoolean False)

    it "performs less than comparison" $ do
      let env = []
      handleCall env "<" [AstInteger 3, AstInteger 5] `shouldBe` Just (AstBoolean True)
      handleCall env "<" [AstInteger 5, AstInteger 3] `shouldBe` Just (AstBoolean False)
      handleCall env "<" [AstInteger 5, AstInteger 5] `shouldBe` Just (AstBoolean False)

    it "works with variables from environment" $ do
      let env = [(AstSymbol "x", AstInteger 10), (AstSymbol "y", AstInteger 3)]
      handleCall env "+" [AstSymbol "x", AstSymbol "y"] `shouldBe` Just (AstInteger 13)
      handleCall env "<" [AstSymbol "y", AstSymbol "x"] `shouldBe` Just (AstBoolean True)

    it "returns Nothing for invalid operations" $ do
      let env = []
      handleCall env "unknown" [AstInteger 1, AstInteger 2] `shouldBe` Nothing
      handleCall env "+" [AstInteger 1] `shouldBe` Nothing
      handleCall env "+" [] `shouldBe` Nothing
      handleCall env "+" [AstBoolean True, AstInteger 2] `shouldBe` Nothing

  describe "evalAST" $ do
    it "evaluates integers to themselves" $ do
      let env = []
      evalAST env (AstInteger 42) `shouldBe` Just (AstInteger 42)

    it "evaluates booleans to themselves" $ do
      let env = []
      evalAST env (AstBoolean True) `shouldBe` Just (AstBoolean True)
      evalAST env (AstBoolean False) `shouldBe` Just (AstBoolean False)

    it "evaluates symbols" $ do
      let env = [(AstSymbol "x", AstInteger 5)]
      evalAST env (AstSymbol "x") `shouldBe` Just (AstInteger 5)
      evalAST env (AstSymbol "#t") `shouldBe` Just (AstBoolean True)

    it "evaluates function calls" $ do
      let env = []
      evalAST env (Call "+" [AstInteger 1, AstInteger 2]) `shouldBe` Just (AstInteger 3)

    it "evaluates if expressions" $ do
      let env = []
      evalAST env (If (AstBoolean True) (AstInteger 1) (AstInteger 0)) `shouldBe` Just (AstInteger 1)
      evalAST env (If (AstBoolean False) (AstInteger 1) (AstInteger 0)) `shouldBe` Just (AstInteger 0)

    it "evaluates lambda expressions" $ do
      let env = []
      let lambda = Lambda [AstSymbol "x"] (AstInteger 1)
      evalAST env lambda `shouldBe` Just lambda

    it "evaluates define expressions" $ do
      let env = []
      evalAST env (Define (AstSymbol "x") (AstInteger 5)) `shouldBe` Just (AstSymbol "")

    it "evaluates empty lists" $ do
      let env = []
      evalAST env (AstList []) `shouldBe` Just (AstList [])

    it "evaluates single-element lists" $ do
      let env = []
      evalAST env (AstList [AstInteger 42]) `shouldBe` Just (AstInteger 42)

    it "evaluates function calls in lists" $ do
      let env = []
      evalAST env (AstList [AstSymbol "+", AstInteger 1, AstInteger 2]) `shouldBe` Just (AstInteger 3)

  describe "evalASTWithEnv" $ do
    it "returns Nothing for empty list" $ do
      evalASTWithEnv [] [] `shouldBe` Nothing

    it "evaluates single expression" $ do
      evalASTWithEnv [] [AstInteger 42] `shouldBe` Just (AstInteger 42)

    it "handles define statements" $ do
      let exprs = [Define (AstSymbol "x") (AstInteger 5), AstSymbol "x"]
      evalASTWithEnv [] exprs `shouldBe` Just (AstInteger 5)

    it "handles not good define value" $ do
      let exprs = [Define (AstSymbol "x") (Call "+" [AstSymbol "foo", AstInteger 1]), AstSymbol "x"]
      evalASTWithEnv [] exprs `shouldBe` Nothing

    it "handles double define value" $ do
      let exprs = [Define (AstSymbol "x") (AstInteger 2), Define (AstSymbol "z") (AstSymbol "x"), AstSymbol "z"]
      evalASTWithEnv [] exprs `shouldBe` Just (AstInteger 2)

    it "handles multiple defines" $ do
      let exprs = [Define (AstSymbol "x") (AstInteger 5),
                  Define (AstSymbol "y") (AstInteger 10),
                  Call "+" [AstSymbol "x", AstSymbol "y"]]
      evalASTWithEnv [] exprs `shouldBe` Just (AstInteger 15)

    it "evaluates non-define expressions in sequence" $ do
      let exprs = [AstInteger 1, AstInteger 2, AstInteger 42]
      evalASTWithEnv [] exprs `shouldBe` Just (AstInteger 42)

    it "handles failed define evaluation" $ do
      let exprs = [Define (AstSymbol "x") (Call "+" [AstBoolean True, AstInteger 1])]
      evalASTWithEnv [] exprs `shouldBe` Just (AstSymbol "")

    it "handles failed expression evaluation" $ do
      let exprs = [AstInteger 42, Call "+" [AstBoolean True, AstInteger 1]]
      evalASTWithEnv [] exprs `shouldBe` Nothing

  describe "handleCondition (via If evaluation)" $ do
    it "evaluates true condition" $ do
      let env = []
      let ifExpr = If (AstBoolean True) (AstInteger 1) (AstInteger 0)
      evalAST env ifExpr `shouldBe` Just (AstInteger 1)

    it "evaluates false condition" $ do
      let env = []
      let ifExpr = If (AstBoolean False) (AstInteger 1) (AstInteger 0)
      evalAST env ifExpr `shouldBe` Just (AstInteger 0)

    it "handles non-boolean conditions" $ do
      let env = []
      let ifExpr = If (AstInteger 42) (AstInteger 1) (AstInteger 0)
      evalAST env ifExpr `shouldBe` Nothing

    it "evaluates complex conditions" $ do
      let env = [(AstSymbol "x", AstInteger 5)]
      let condition = Call "<" [AstSymbol "x", AstInteger 10]
      let ifExpr = If condition (AstSymbol "x") (AstInteger 0)
      evalAST env ifExpr `shouldBe` Just (AstInteger 5)

  describe "Show instance (deriving)" $ do
    it "shows AstInteger values using Show" $ do
      show (AstInteger 42) `shouldBe` "AstInteger 42"
      show (AstInteger (-17)) `shouldBe` "AstInteger (-17)"

    it "shows AstBoolean values using Show" $ do
      show (AstBoolean True) `shouldBe` "AstBoolean True"
      show (AstBoolean False) `shouldBe` "AstBoolean False"

    it "shows AstSymbol values using Show" $ do
      show (AstSymbol "test") `shouldBe` "AstSymbol \"test\""
      show (AstSymbol "") `shouldBe` "AstSymbol \"\""

    it "shows AstList values using Show" $ do
      show (AstList []) `shouldBe` "AstList []"
      show (AstList [AstInteger 1]) `shouldBe` "AstList [AstInteger 1]"
      show (AstList [AstInteger 1, AstSymbol "x"]) `shouldBe` 
        "AstList [AstInteger 1,AstSymbol \"x\"]"

    it "shows Define expressions using Show" $ do
      show (Define (AstSymbol "x") (AstInteger 5)) `shouldBe` 
        "Define (AstSymbol \"x\") (AstInteger 5)"

    it "shows Call expressions using Show" $ do
      show (Call "+" [AstInteger 1, AstInteger 2]) `shouldBe` 
        "Call \"+\" [AstInteger 1,AstInteger 2]"

    it "shows Lambda expressions using Show" $ do
      show (Lambda [AstSymbol "x"] (AstInteger 1)) `shouldBe` 
        "Lambda [AstSymbol \"x\"] (AstInteger 1)"

    it "shows If expressions using Show" $ do
      show (If (AstBoolean True) (AstInteger 1) (AstInteger 0)) `shouldBe` 
        "If (AstBoolean True) (AstInteger 1) (AstInteger 0)"

    it "shows complex nested structures using Show" $ do
      let complex = Call "+" [Define (AstSymbol "x") (AstInteger 5), 
                             Lambda [AstSymbol "y"] (AstSymbol "y")]
      show complex `shouldBe` 
        "Call \"+\" [Define (AstSymbol \"x\") (AstInteger 5),Lambda [AstSymbol \"y\"] (AstSymbol \"y\")]"

  describe "Eq instance (deriving)" $ do
    it "compares AstInteger values for equality" $ do
      AstInteger 42 `shouldBe` AstInteger 42
      AstInteger 42 `shouldNotBe` AstInteger 43

    it "compares AstBoolean values for equality" $ do
      AstBoolean True `shouldBe` AstBoolean True
      AstBoolean True `shouldNotBe` AstBoolean False

    it "compares AstSymbol values for equality" $ do
      AstSymbol "test" `shouldBe` AstSymbol "test"
      AstSymbol "test" `shouldNotBe` AstSymbol "other"

    it "compares AstList values for equality" $ do
      AstList [AstInteger 1] `shouldBe` AstList [AstInteger 1]
      AstList [AstInteger 1] `shouldNotBe` AstList [AstInteger 2]

    it "compares Define expressions for equality" $ do
      Define (AstSymbol "x") (AstInteger 5) `shouldBe` Define (AstSymbol "x") (AstInteger 5)
      Define (AstSymbol "x") (AstInteger 5) `shouldNotBe` Define (AstSymbol "y") (AstInteger 5)

    it "compares Call expressions for equality" $ do
      Call "+" [AstInteger 1, AstInteger 2] `shouldBe` Call "+" [AstInteger 1, AstInteger 2]
      Call "+" [AstInteger 1, AstInteger 2] `shouldNotBe` Call "-" [AstInteger 1, AstInteger 2]

    it "compares Lambda expressions for equality" $ do
      Lambda [AstSymbol "x"] (AstInteger 1) `shouldBe` Lambda [AstSymbol "x"] (AstInteger 1)
      Lambda [AstSymbol "x"] (AstInteger 1) `shouldNotBe` Lambda [AstSymbol "y"] (AstInteger 1)

    it "compares If expressions for equality" $ do
      If (AstBoolean True) (AstInteger 1) (AstInteger 0) `shouldBe` 
        If (AstBoolean True) (AstInteger 1) (AstInteger 0)
      If (AstBoolean True) (AstInteger 1) (AstInteger 0) `shouldNotBe` 
        If (AstBoolean False) (AstInteger 1) (AstInteger 0)

    it "compares complex nested structures for equality" $ do
      let expr1 = Lambda [AstSymbol "x"] (Call "+" [AstSymbol "x", AstInteger 1])
      let expr2 = Lambda [AstSymbol "x"] (Call "+" [AstSymbol "x", AstInteger 1])
      let expr3 = Lambda [AstSymbol "y"] (Call "+" [AstSymbol "y", AstInteger 1])
      expr1 `shouldBe` expr2
      expr1 `shouldNotBe` expr3