#!/bin/sh
set -eu

script_dir="$(CDPATH= cd "$(dirname "$0")" && pwd)"
fixture_dir="$script_dir/compile-fail"
checked=0

for fixture in "$fixture_dir"/*.idr; do
  expected="$(sed -n 's/^-- EXPECT: //p' "$fixture")"
  if [ -z "$expected" ]; then
    echo "Missing expected diagnostic marker: $fixture" >&2
    exit 1
  fi

  output="$(cd "$fixture_dir" && idris2 --package iotaTime --check "$(basename "$fixture")" 2>&1)" && succeeded=true || succeeded=false
  if [ "$succeeded" = true ]; then
    echo "Expected compilation to fail: $fixture" >&2
    exit 1
  fi
  if ! printf '%s\n' "$output" | grep -Fq "$expected"; then
    echo "Compilation failed for the wrong reason: $fixture" >&2
    printf '%s\n' "$output" >&2
    exit 1
  fi
  checked=$((checked + 1))
done

echo "Compile-fail tests: $checked/$checked passed"
