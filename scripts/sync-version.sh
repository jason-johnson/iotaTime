#!/bin/sh
set -eu

repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
full_package="$repo_root/iotaTime.ipkg"
pure_package="$repo_root/iotaTime-pure.ipkg"
makefile="$repo_root/Makefile"

usage() {
  echo "Usage: scripts/sync-version.sh --set X.Y.Z | --check [X.Y.Z]" >&2
  exit 2
}

validate_version() {
  line_count=$(printf '%s' "$1" | awk 'END { print NR }')
  if [ "$line_count" -ne 1 ] || ! printf '%s\n' "$1" | grep -Eq '^[0-9]+\.[0-9]+\.[0-9]+$'; then
    echo "Invalid package version: $1 (expected X.Y.Z)" >&2
    exit 1
  fi
}

package_version() {
  sed -n 's/^version = //p' "$1"
}

make_version() {
  sed -n 's/^PACKAGE_VERSION := //p' "$makefile"
}

check_versions() {
  expected=${1:-}
  full=$(package_version "$full_package")
  pure=$(package_version "$pure_package")
  native=$(make_version)

  validate_version "$full"
  validate_version "$pure"
  validate_version "$native"
  if [ "$pure" != "$full" ] || [ "$native" != "$full" ]; then
    echo "Version declarations disagree: iotaTime=$full iotaTime-pure=$pure Makefile=$native" >&2
    exit 1
  fi
  if [ -n "$expected" ] && [ "$full" != "$expected" ]; then
    echo "Package version is $full; expected $expected" >&2
    exit 1
  fi
  printf '%s\n' "$full"
}

set_version() {
  version=$1
  validate_version "$version"
  validate_version "$(package_version "$full_package")"
  validate_version "$(package_version "$pure_package")"
  validate_version "$(make_version)"

  full_tmp="$full_package.tmp.$$"
  pure_tmp="$pure_package.tmp.$$"
  make_tmp="$makefile.tmp.$$"
  trap 'rm -f "$full_tmp" "$pure_tmp" "$make_tmp"' EXIT HUP INT TERM

  sed "s/^version = .*/version = $version/" "$full_package" > "$full_tmp"
  sed "s/^version = .*/version = $version/" "$pure_package" > "$pure_tmp"
  sed "s/^PACKAGE_VERSION := .*/PACKAGE_VERSION := $version/" "$makefile" > "$make_tmp"

  mv "$full_tmp" "$full_package"
  mv "$pure_tmp" "$pure_package"
  mv "$make_tmp" "$makefile"
  trap - EXIT HUP INT TERM

  check_versions "$version"
}

case ${1:-} in
  --set)
    [ "$#" -eq 2 ] || usage
    set_version "$2"
    ;;
  --check)
    [ "$#" -le 2 ] || usage
    if [ "$#" -eq 2 ]; then
      validate_version "$2"
      check_versions "$2"
    else
      check_versions
    fi
    ;;
  *)
    usage
    ;;
esac
