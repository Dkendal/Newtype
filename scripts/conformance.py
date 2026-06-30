#!/usr/bin/env python3
"""Conformance-test runner: cross-check newtype against tsgo (tsc 7.0).

For each `assert` inside a `unittest`, the newtype compiler is BOTH a runner
(it prints an ok/FAILED report to stderr) and a code generator (with
`--generate-tests --source-comments` it emits a TypeScript file in which every
assert becomes a `type _newtype_test__… = Assert<…>` alias, tagged with a
`/** @newtype line:N */` comment pointing back at the originating source line).

This script feeds the same `.nt` source to both oracles and checks that they
AGREE per assertion -- i.e. newtype reports an assertion as passing iff tsgo
type-checks the corresponding alias without error. A "both fail" result is
still agreement; only a PASS/FAIL split is a disagreement.

Algorithm (matches the runner spec):
  1. Run `./target/debug/newtype --generate-tests --source-comments < FILE`,
     capturing stdout (the TypeScript) and stderr (the ok/FAILED report).
  2. Parse stdout for `/** @newtype line:N */` followed by `type NAME = …` to
     build the test universe (NAME -> source line N).
  3. Parse stderr for `--> LINE:COL` pointers -> the set of source lines newtype
     reports as FAILED.
  4. Write the TypeScript to a temp `.ts` file and run `tsgo --noEmit --strict`.
  5. For each tsgo error `…:TSLINE:COL - error TS…`, scan UPWARD in the temp
     file from TSLINE to the nearest `/** @newtype line:N */` comment -> the set
     of source lines tsgo reports as FAILED.
  6. Compare PASS-agreement per test, print a trace, and exit nonzero if any
     test disagrees (or if either oracle could not be run).

Usage:
    python3 scripts/conformance.py [FILE.nt ...]

With no arguments it runs a sensible default set (examples/test.nt and every
`tests/conformance/*.nt` fixture). The newtype binary is built first
(`cargo build`) and invoked directly via stdin, so the binary never sees a
filename -- source comments are line-only by design.
"""

import argparse
import glob
import os
import re
import shutil
import subprocess
import sys
import tempfile
from collections import Counter

REPO_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
BINARY = os.path.join(REPO_ROOT, "target", "debug", "newtype")

ANSI_RE = re.compile(r"\x1b\[[0-9;]*m")
# A generated alias comment, e.g. `/** @newtype line:35 */`.
COMMENT_RE = re.compile(r"/\*\*\s*@newtype\s+line:(\d+)\s*\*/")
# A generated alias declaration, e.g. `type _newtype_test__foo_0 = …`.
TYPE_RE = re.compile(r"^type\s+(_newtype_test__\w+)\s*=")
# newtype's failure pointer in the stderr report, e.g. `  --> 35:10`.
NT_FAIL_RE = re.compile(r"-->\s+(\d+):\d+")
# A tsgo diagnostic line, e.g. `/abs/psub.ts:84:49 - error TS2344: …`.
TSGO_ERR_RE = re.compile(r":(\d+):(\d+)\s+-\s+error\s+TS\d+")


def strip_ansi(text):
    return ANSI_RE.sub("", text)


def find_tsgo():
    """Locate tsgo: PATH first, then `mise which tsgo`, then mise install dirs."""
    found = shutil.which("tsgo")
    if found:
        return found

    try:
        out = subprocess.run(
            ["mise", "which", "tsgo"],
            capture_output=True,
            text=True,
            timeout=30,
        )
        candidate = out.stdout.strip()
        if out.returncode == 0 and candidate and os.path.exists(candidate):
            return candidate
    except (OSError, subprocess.SubprocessError):
        pass

    home = os.path.expanduser("~")
    patterns = [
        os.path.join(home, ".local", "share", "mise", "installs", "node", "*", "bin", "tsgo"),
        os.path.join(home, ".local", "share", "mise", "installs", "*", "*", "bin", "tsgo"),
    ]
    for pattern in patterns:
        matches = sorted(glob.glob(pattern))
        if matches:
            return matches[-1]
    return None


def build_binary():
    print("building newtype (cargo build)…", file=sys.stderr)
    result = subprocess.run(
        ["cargo", "build"],
        cwd=REPO_ROOT,
        capture_output=True,
        text=True,
    )
    if result.returncode != 0:
        sys.stderr.write(result.stdout)
        sys.stderr.write(result.stderr)
        raise SystemExit("cargo build failed")


def run_newtype(source):
    """Return (stdout_ts, stderr_report, returncode). A nonzero exit can simply
    mean an assertion failed (which is what we inspect), but with no generated
    output it signals a parse/compile error — the caller distinguishes the two."""
    proc = subprocess.run(
        [BINARY, "--generate-tests", "--source-comments"],
        input=source,
        capture_output=True,
        text=True,
        cwd=REPO_ROOT,
    )
    return proc.stdout, proc.stderr, proc.returncode


def parse_test_universe(ts_stdout):
    """Walk the emitted TypeScript, pairing each `/** @newtype line:N */` comment
    with the `type NAME = …` line that immediately follows it.

    Returns a list of dicts {name, src_line} in emission order."""
    tests = []
    pending_line = None
    for raw in ts_stdout.splitlines():
        line = raw.strip()
        m = COMMENT_RE.search(line)
        if m:
            pending_line = int(m.group(1))
            continue
        t = TYPE_RE.match(line)
        if t and pending_line is not None:
            tests.append({"name": t.group(1), "src_line": pending_line})
            pending_line = None
    return tests


def parse_newtype_failures(stderr_report):
    """Source lines newtype flagged FAILED, from `--> LINE:COL` pointers."""
    return {int(m.group(1)) for m in NT_FAIL_RE.finditer(strip_ansi(stderr_report))}


def parse_tsgo_failures(ts_text, tsgo_stdout):
    """Map each tsgo error to the source line of the nearest preceding
    `/** @newtype line:N */` comment in the emitted TypeScript.

    `ts_text` is the exact content written to the temp file; `tsgo_stdout` is
    tsgo's (ANSI-stripped) diagnostic output. Returns (failures, unattributed):
    a set of source lines, and a list of any error lines that could NOT be tied
    to a generated test (e.g. an error in a preserved interface or the helper
    fence) — those are setup problems the caller must surface, not assertion
    failures."""
    ts_lines = ts_text.split("\n")
    failures = set()
    unattributed = []
    for raw in strip_ansi(tsgo_stdout).splitlines():
        m = TSGO_ERR_RE.search(raw)
        if not m:
            continue
        ts_line = int(m.group(1))  # 1-based line in the temp file
        # Scan upward (inclusive of the error line) for the generated test's
        # comment, stopping at a blank line — a statement boundary — so an error
        # in a non-generated declaration isn't misattributed to an earlier test.
        idx = min(ts_line - 1, len(ts_lines) - 1)
        src = None
        while idx >= 0:
            c = COMMENT_RE.search(ts_lines[idx])
            if c:
                src = int(c.group(1))
                break
            if ts_lines[idx].strip() == "":
                break
            idx -= 1
        if src is not None:
            failures.add(src)
        else:
            unattributed.append(raw.strip())
    return failures, unattributed


def run_file(path, tsgo):
    print("=" * 72)
    print(f"conformance: {path}")
    print("=" * 72)

    with open(path, "r") as f:
        source = f.read()

    ts_stdout, nt_stderr, nt_rc = run_newtype(source)
    tests = parse_test_universe(ts_stdout)
    nt_failures = parse_newtype_failures(nt_stderr)

    if not tests:
        # No generated tests AND a nonzero exit means the compiler itself failed
        # (parse/compile error), not "this file has no unittests" — surface it.
        if nt_rc != 0:
            print(f"  ERROR: newtype exited {nt_rc} with no generated tests "
                  f"(likely a parse/compile error):")
            sys.stderr.write(nt_stderr)
            return False
        print("  (no unittest assertions found)")
        return True

    # Verdicts are keyed by source line, so two asserts sharing one physical line
    # would collapse and could mask a genuine PASS/FAIL split. Require one assert
    # per line rather than silently mis-scoring.
    line_counts = Counter(t["src_line"] for t in tests)
    ambiguous = sorted(line for line, count in line_counts.items() if count > 1)
    if ambiguous:
        print(f"  ERROR: multiple asserts map to source line(s) {ambiguous}; "
              f"put one assert per line so verdicts stay unambiguous.")
        return False

    # Write the emitted TypeScript to a temp .ts file for tsgo (no stdin mode).
    tmp = tempfile.NamedTemporaryFile(
        mode="w", suffix=".ts", prefix="newtype_conformance_", delete=False
    )
    try:
        tmp.write(ts_stdout)
        tmp.close()
        tsgo_proc = subprocess.run(
            [tsgo, "--noEmit", "--strict", tmp.name],
            capture_output=True,
            text=True,
            cwd=REPO_ROOT,
        )
        tsgo_out = tsgo_proc.stdout + "\n" + tsgo_proc.stderr
        tsgo_rc = tsgo_proc.returncode
        tsgo_failures, tsgo_unattributed = parse_tsgo_failures(ts_stdout, tsgo_out)
    finally:
        os.unlink(tmp.name)

    # A tsgo error we can't tie to a generated test, or a nonzero exit with no
    # parseable diagnostics at all, is a setup failure — not an assertion result.
    setup_error = False
    if tsgo_unattributed:
        print("  ERROR: tsgo reported error(s) not attributable to any generated test:")
        for err in tsgo_unattributed:
            print(f"      {err}")
        setup_error = True
    if tsgo_rc != 0 and not tsgo_failures and not tsgo_unattributed:
        print(f"  ERROR: tsgo exited {tsgo_rc} but produced no parseable diagnostics.")
        setup_error = True

    all_agree = True
    print(f"  {'SRC':>4}  {'newtype':<8} {'tsgo':<8} {'verdict':<10} assertion")
    print(f"  {'-'*4}  {'-'*8} {'-'*8} {'-'*10} {'-'*40}")
    for test in tests:
        line = test["src_line"]
        nt_pass = line not in nt_failures
        tsgo_pass = line not in tsgo_failures
        agree = nt_pass == tsgo_pass
        all_agree = all_agree and agree
        verdict = "AGREE" if agree else "DISAGREE"
        print(
            f"  {line:>4}  {'PASS' if nt_pass else 'FAIL':<8} "
            f"{'PASS' if tsgo_pass else 'FAIL':<8} {verdict:<10} {test['name']}"
        )

    passed = sum(1 for t in tests if t["src_line"] not in nt_failures)
    status = "ALL AGREE" if all_agree else "DISAGREEMENT DETECTED"
    if setup_error:
        status = "SETUP ERROR"
    print(f"\n  {len(tests)} assertion(s): newtype {passed} pass / "
          f"{len(tests) - passed} fail; {status}")
    return all_agree and not setup_error


def default_files():
    files = []
    example = os.path.join(REPO_ROOT, "examples", "test.nt")
    if os.path.exists(example):
        files.append(example)
    files.extend(sorted(glob.glob(os.path.join(REPO_ROOT, "tests", "conformance", "*.nt"))))
    return files


def main():
    parser = argparse.ArgumentParser(
        description="Cross-check newtype assertions against tsgo (tsc 7.0)."
    )
    parser.add_argument(
        "files",
        nargs="*",
        help="newtype source files to check (default: examples/test.nt and tests/conformance/*.nt)",
    )
    args = parser.parse_args()

    files = args.files if args.files else default_files()
    if not files:
        raise SystemExit("no input files")

    tsgo = find_tsgo()
    if not tsgo:
        raise SystemExit("could not locate tsgo (not on PATH, not under mise)")
    print(f"using tsgo: {tsgo}", file=sys.stderr)

    build_binary()
    if not os.path.exists(BINARY):
        raise SystemExit(f"binary not found after build: {BINARY}")

    all_ok = True
    for path in files:
        if not os.path.exists(path):
            print(f"skip (missing): {path}", file=sys.stderr)
            all_ok = False
            continue
        all_ok = run_file(path, tsgo) and all_ok

    print()
    if all_ok:
        print("RESULT: newtype and tsgo agree on every assertion.")
        sys.exit(0)
    else:
        print("RESULT: disagreement (or setup error) detected.")
        sys.exit(1)


if __name__ == "__main__":
    main()
