#!/usr/bin/env python3

import subprocess
from pathlib import Path

OUT_BIN = "./out.bin"

EXAMPLE_FILES = [
    "RuneLang/examples/fibonacci.ru",
    "RuneLang/examples/hello_rune.ru",
    "RuneLang/examples/if_else.ru",
    "RuneLang/examples/loops.ru",
    "RuneLang/examples/return_0.ru",
    "RuneLang/examples/strings.ru",
]

EXPECTED_STDIN = {
    "RuneLang/examples/fibonacci.ru": None,
    "RuneLang/examples/hello_rune.ru": None,
    "RuneLang/examples/if_else.ru": None,
    "RuneLang/examples/loops.ru": None,
    "RuneLang/examples/return_0.ru": None,
    "RuneLang/examples/strings.ru": None,
}

EXPECTED_STDOUT = {
    "RuneLang/examples/fibonacci.ru": "",
    "RuneLang/examples/hello_rune.ru": "Hello, Rune!\n",
    "RuneLang/examples/if_else.ru": "a is less than 10\n",
    "RuneLang/examples/loops.ru": "The final value is: 12\n",
    "RuneLang/examples/return_0.ru": "",
    "RuneLang/examples/strings.ru": "Hello, Rune!\n",
}

EXPECTED_RETURN = {
    "RuneLang/examples/fibonacci.ru": 55,
    "RuneLang/examples/hello_rune.ru": 0,
    "RuneLang/examples/if_else.ru": 5,
    "RuneLang/examples/loops.ru": 0,
    "RuneLang/examples/return_0.ru": 0,
    "RuneLang/examples/strings.ru": 0,
}

GREEN = "\033[32m"
RED = "\033[31m"
BOLD = "\033[1m"
RESET = "\033[0m"
CHECK = "✓"
CROSS = "✗"

ERROR = 84


def compile(filename: str, output_bin: str = OUT_BIN) -> str:
    file = Path(filename)
    asm = file.with_suffix(".asm")
    obj = file.with_suffix(".o")

    subprocess.run(["./rune", "build", str(file), "-o", str(asm)], check=True)
    subprocess.run(["nasm", "-f", "elf64", str(asm), "-o", str(obj)], check=True)
    subprocess.run(["gcc", "-no-pie", str(obj), "-o", output_bin], check=True)

    return output_bin


def run_bin(stdin_data: bytes):
    process = subprocess.Popen(
        OUT_BIN,
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    stdout, _ = process.communicate(stdin_data)
    return stdout.decode(), process.returncode


def run_test(example_file):
    expected_stdin = EXPECTED_STDIN[example_file]
    stdin_data = (
        Path(example_file).read_bytes()
        if expected_stdin is None
        else expected_stdin.encode()
    )

    out, code = run_bin(stdin_data)
    expected_out = EXPECTED_STDOUT[example_file]
    expected_code = EXPECTED_RETURN[example_file]

    ok_out = out == expected_out
    ok_code = code == expected_code

    return ok_out and ok_code, out, code, expected_out, expected_code


def main():
    print(f"{BOLD}Running tests…{RESET}")
    failed = 0

    for example in EXAMPLE_FILES:
        compile(example)
        ok, out, code, exp_out, exp_code = run_test(example)

        name = example.rpartition("/")[-1]

        if ok:
            print(f" {GREEN}{CHECK}{RESET} {name}")
        else:
            failed += 1
            print(f" {RED}{CROSS}{RESET} {name}")
            print(f"    stdout got: {repr(out)}")
            print(f"    stdout exp: {repr(exp_out)}")
            print(f"    code   got: {code}")
            print(f"    code   exp: {exp_code}")

    if failed:
        print(f"\n{RED}{failed} test(s) failed{RESET}")
        exit(ERROR)
    else:
        print(f"\n{GREEN}All tests passed{RESET}")


if __name__ == "__main__":
    main()
