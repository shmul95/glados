module IR.IRSpec (irTests) where

import Rune.AST.Parser (parseRune)
import Rune.IR.Generator (generateIR)
import Rune.IR.Printer (prettyPrintIR)
import Rune.Lexer.Lexer (lexer)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Text.Megaparsec (errorBundlePretty)

--
-- public
--

irTests :: TestTree
irTests =
  testGroup
    "IR.IRSpec"
    [ testCase "IR Test Programs" irTestPrograms
    ]

--
-- private
--

--
-- helpers
--

runIR :: String -> String
runIR source =
  case lexer "test" source of
    Left err -> error $ errorBundlePretty err
    Right tokens -> case parseRune "test" tokens of
      Left err -> error err
      Right ast -> prettyPrintIR $ generateIR ast

--
-- test programs
--

irTestPrograms :: IO ()
irTestPrograms = do
  let program =
        unlines
          [ "def show_string(str: string) -> null",
            "{",
            "    for c in str {",
            "        show(c);",
            "    }",
            "}",
            "",
            "def main() -> null",
            "{",
            "    str: string = \"Hello, Rune!\\n\";",
            "    show_string(str);",
            "}"
          ]

  let expected =
        unlines
          [ "GLOBAL str_main0: string = \"Hello, Rune!\\n\\0\"",
            "DEF show_string(p_str: *u8):",
            "    p_ptr0: *u8 = p_str",
            ".L.loop_header0:",
            "    c: u8 = DEREF p_ptr0",
            ".L.loop_check0:",
            "    JUMP_EQ0 c, .L.loop_end0",
            ".L.body0:",
            "    CALL putchar(c)",
            "    INC p_ptr0",
            "    JUMP .L.loop_header0",
            ".L.loop_end0:",
            "    RET",
            "DEF main():",
            "    p_ptr0: *u8 = ADDR str_main0",
            "    t1: i32 = CALL show_string(p_ptr0)",
            "    RET"
          ]

  runIR program @?= expected
