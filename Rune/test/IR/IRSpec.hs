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
    [ testCase "IR Test Programs" irTestShowString,
      testCase "IR For To Loop" irTestForTo,
      testCase "IR Loop Control Statements" irTestLoopControl,
      testCase "IR Conditional Statements" irTestConditional
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

irTestShowString :: IO ()
irTestShowString = do
  let program =
        unlines
          [ "def show_string(str: string) -> null",
            "{",
            "    for c in str {",
            "        if c == 'a' {",
            "            return;",
            "        }",
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
            "    t1 = CMP_EQ c, 'a'",
            "    JUMP_FALSE t1, .L.end1",
            "    RET",
            ".L.end1:",
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

irTestForTo :: IO ()
irTestForTo = do
  let program =
        unlines
          [ "def main() -> null",
            "{",
            "    for i = 0 to 10 {",
            "        ++i;",
            "    }",
            "}"
          ]
  let expected =
        unlines
          [ "DEF main():",
            "    i: i32 = 0",
            ".L.loop_header0:",
            "    cmp0 = CMP_LT i, 10",
            "    JUMP_FALSE cmp0, .L.loop_end0",
            ".L.body0:",
            "    INC i",
            "    JUMP .L.loop_header0",
            ".L.loop_end0:",
            "    RET"
          ]
  runIR program @?= expected

irTestLoopControl :: IO ()
irTestLoopControl = do
  let program =
        unlines
          [ "def main() -> null",
            "{",
            "    k: i32 = 0;",
            "",
            "    loop {",
            "        k += 2;",
            "        if k > 10 {",
            "            stop;",
            "        }",
            "        if k % 4 == 0 {",
            "            next;",
            "        }",
            "        show(k);",
            "    }",
            "}"
          ]
  let expected =
        unlines
          [ "DEF main():",
            "    k: i32 = 0",
            ".L.loop_header0:",
            "    t0: i32 = ADD k, 2",
            "    k: i32 = t0",
            "    t1 = CMP_GT k, 10",
            "    JUMP_FALSE t1, .L.end1",
            "    JUMP .L.loop_end",
            ".L.end1:",
            "    t2: i32 = MOD k, 4",
            "    t3 = CMP_EQ t2, 0",
            "    JUMP_FALSE t3, .L.end2",
            "    JUMP .L.loop_header",
            ".L.end2:",
            "    CALL printf(k)",
            "    JUMP .L.loop_header0",
            ".L.loop_end0:",
            "    RET"
          ]
  runIR program @?= expected

irTestConditional :: IO ()
irTestConditional = do
  let program =
        unlines
          [ "def main() -> i32",
            "{",
            "    a: i32 = 5;",
            "",
            "    if a < 10 {",
            "        show(\"a is less than 10\");",
            "    } else {",
            "        show(\"a is 10 or greater\");",
            "    }",
            "    return a;",
            "}"
          ]
  let expected =
        unlines
          [ "GLOBAL str_main0: string = \"a is less than 10\\0\"",
            "GLOBAL str_main1: string = \"a is 10 or greater\\0\"",
            "DEF main():",
            "    a: i32 = 5",
            "    t0 = CMP_LT a, 10",
            "    JUMP_FALSE t0, .L.else0",
            "    p_ptr1: *u8 = ADDR str_main0",
            "    CALL puts(p_ptr1)",
            "    JUMP .L.end0",
            ".L.else0:",
            "    p_ptr2: *u8 = ADDR str_main1",
            "    CALL puts(p_ptr2)",
            ".L.end0:",
            "    RET a"
          ]
  runIR program @?= expected
