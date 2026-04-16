#!/usr/bin/env bash
# build_eval_copy.sh
#
# Copies the throughline source tree to ../throughline-eval, stripping all
# Haskell comments and omitting documentation, so the copy can be used as an
# evaluation target without leaking CLAUDE.md or other context into a session.
#
# Usage: bash utils/build_eval_copy.sh

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$(dirname "$SCRIPT_DIR")"
TARGET_DIR="$(dirname "$REPO_DIR")/throughline-eval"

echo "Source : $REPO_DIR"
echo "Target : $TARGET_DIR"

rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"

rsync -a \
  --exclude='.git/' \
  --exclude='.stack-work/' \
  --exclude='.claude/' \
  --exclude='.crunch/' \
  --exclude='.github/' \
  --exclude='sessions/' \
  --exclude='utils/' \
  --exclude='*.md' \
  --exclude='LICENSE' \
  --exclude='hlint' \
  "$REPO_DIR/" "$TARGET_DIR/"

echo "Stripping comments from Haskell sources..."

python3 - "$TARGET_DIR" <<'PYEOF'
import sys, os, re

def strip_haskell_comments(src):
    result = []
    i = 0
    n = len(src)

    while i < n:
        # String literal — pass through verbatim, respecting backslash escapes
        if src[i] == '"':
            result.append(src[i])
            i += 1
            while i < n:
                if src[i] == '\\' and i + 1 < n:
                    result.append(src[i:i+2])
                    i += 2
                elif src[i] == '"':
                    result.append(src[i])
                    i += 1
                    break
                else:
                    result.append(src[i])
                    i += 1
            continue

        # Pragma: {-# ... #-} — preserve (controls compilation)
        if src[i:i+3] == '{-#':
            end = src.find('#-}', i + 3)
            if end == -1:
                result.append(src[i:])
                break
            result.append(src[i:end+3])
            i = end + 3
            continue

        # Block comment: {- ... -} with nesting support
        if src[i:i+2] == '{-':
            depth = 1
            i += 2
            while i < n and depth > 0:
                if src[i:i+2] == '{-':
                    depth += 1
                    i += 2
                elif src[i:i+2] == '-}':
                    depth -= 1
                    i += 2
                else:
                    if src[i] == '\n':
                        result.append('\n')
                    i += 1
            continue

        # Line comment: -- ...
        if src[i:i+2] == '--':
            while i < n and src[i] != '\n':
                i += 1
            continue

        result.append(src[i])
        i += 1

    return ''.join(result)

def collapse_blank_lines(src):
    return re.sub(r'\n{3,}', '\n\n', src)

target = sys.argv[1]
count = 0
for root, dirs, files in os.walk(target):
    dirs[:] = [d for d in dirs if d != '.stack-work']
    for fname in files:
        if fname.endswith('.hs'):
            path = os.path.join(root, fname)
            with open(path, 'r', encoding='utf-8') as f:
                src = f.read()
            stripped = collapse_blank_lines(strip_haskell_comments(src))
            with open(path, 'w', encoding='utf-8') as f:
                f.write(stripped)
            count += 1

print(f"  Processed {count} .hs files.")
PYEOF

echo "Done. Eval copy at: $TARGET_DIR"
