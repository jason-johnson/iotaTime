#!/bin/sh
set -eu

script_dir="$(CDPATH= cd "$(dirname "$0")" && pwd)"
fixture_dir="$script_dir/compile-fail"
checked=0

for fixture in "$fixture_dir"/*.idr; do
  if (cd "$fixture_dir" && idris2 --package iotaTime --check "$(basename "$fixture")") >/dev/null 2>&1; then
    echo "Expected compilation to fail: $fixture" >&2
    exit 1
  fi
  checked=$((checked + 1))
done

echo "Compile-fail tests: $checked/$checked passed"
