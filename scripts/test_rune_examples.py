#!/usr/bin/env python3

import subprocess
import os
from typing import List


def glob_files(path: str, extension: str) -> List[str]:
    extension = "." + extension.lstrip(".")
    matches: List[str] = []

    for root, _, files in os.walk(path):
        for name in files:
            if name.endswith(extension):
                matches.append(os.path.join(root, name))

    return sorted(matches)


OUT_BIN = "./out.bin"

EXAMPLE_FILES = glob_files("RuneLang/test/RuneUnitTests", "ru")

EXPECTED_STDOUT = "[+] PASSED\n" * 39
EXPECTED_RETURN = 0

GREEN = "\033[32m"
RED = "\033[31m"
BOLD = "\033[1m"
RESET = "\033[0m"
CHECK = "✓"
CROSS = "✗"

ERROR = 84


def print_ok(message: str) -> None:
    print(f"{GREEN}{CHECK}{RESET} {message}")


def print_ko(message: str) -> None:
    print(f"{RED}{CROSS}{RESET} {message}")


def compile(examples_files: List[str]) -> subprocess.CompletedProcess:

    build_cmd = ["./rune", "build", *examples_files, "-o", OUT_BIN]

    try:
        result = subprocess.run(
            build_cmd,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
        )
    except FileNotFoundError:
        print_ko("`rune` command not found.")
        raise SystemExit(ERROR)

    if result.returncode != 0:
        print_ko("Compilation failed.")
        print(result.stderr)
        raise SystemExit(ERROR)

    print_ok("Compilation succeeded.")
    return result


def verify_output(run: subprocess.CompletedProcess) -> None:

    if run.returncode != EXPECTED_RETURN:
        print_ko(
            f"Program exited with code {run.returncode}, expected {EXPECTED_RETURN}."
        )
        raise SystemExit(ERROR)

    print_ok("Program exited with the expected return code.")


def main() -> None:

    if not EXAMPLE_FILES:
        print_ko("No test files found.")
        raise SystemExit(ERROR)

    run = compile(EXAMPLE_FILES)
    verify_output(run)


if __name__ == "__main__":
    main()
