#!/usr/bin/env python3

import subprocess
import os
import re

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

EXPECTED_STDOUT = re.compile(r"\x1B\[[0-?]*[ -/]*[@-~]")
EXPECTED_RETURN = 0
EXPECTED_LINES = 164

GREEN = "\033[32m"
RED = "\033[31m"
RESET = "\033[0m"
CHECK = "✓"
CROSS = "✗"
VALID = f"[{CHECK}]"

ERROR = 84


def print_ok(message: str) -> None:
    print(f"{GREEN}{CHECK}{RESET} {message}")


def print_ko(message: str) -> None:
    print(f"{RED}{CROSS}{RESET} {message}")


def strip_ansi(text: str) -> str:
    return EXPECTED_STDOUT.sub("", text)


def run_command(command: List[str]) -> subprocess.CompletedProcess:
    try:
        return subprocess.run(
            command,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
        )
    except FileNotFoundError:
        print_ko(f"`{' '.join(command)}` command not found.")
        raise SystemExit(ERROR)


def run_checked(command: List[str], error_message: str) -> None:
    res = run_command(command)
    if res.returncode != 0:
        print_ko(error_message)
        if res.stderr:
            print(res.stderr)
        raise SystemExit(ERROR)


def compile(examples_files: List[str]) -> None:
    run_checked(["make", "lib"], "Library build failed.")
    run_checked(
        ["./rune", "build", *examples_files, "-o", OUT_BIN, "-lstd"],
        "Rune build failed.",
    )

    if not os.path.isfile(OUT_BIN):
        print_ko(f"{OUT_BIN} was not created.")
        raise SystemExit(ERROR)

    os.chmod(OUT_BIN, 0o755)
    print_ok("Compilation succeeded.")


def verify_output(run: subprocess.CompletedProcess) -> None:
    if run.returncode != EXPECTED_RETURN:
        print_ko(
            f"Program exited with code {run.returncode}, expected {EXPECTED_RETURN}."
        )
        raise SystemExit(ERROR)

    cleaned = strip_ansi(run.stdout)
    lines = [line for line in cleaned.splitlines() if line.strip()]

    if len(lines) != EXPECTED_LINES:
        print_ko(f"Expected {EXPECTED_LINES} lines, got {len(lines)}.")
        raise SystemExit(ERROR)

    for i, line in enumerate(lines, 1):
        if not line.startswith(VALID):
            print_ko(f"Test failed at line: {i}: {line}")
            raise SystemExit(ERROR)

    print(run.stdout)
    print_ok("Program output is valid.")


def delete_binary() -> None:
    try:
        os.remove(OUT_BIN)
        print_ok("Temporary binary deleted.")
    except OSError:
        print_ko("Failed to delete temporary binary.")


def main() -> None:
    if not EXAMPLE_FILES:
        print_ko("No test files found.")
        raise SystemExit(ERROR)

    compile(EXAMPLE_FILES)
    run = run_command([OUT_BIN])
    verify_output(run)
    delete_binary()


if __name__ == "__main__":
    main()
