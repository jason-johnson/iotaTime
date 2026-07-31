#!/usr/bin/env bash

set -euo pipefail

readonly IDRIS2_COMMIT="15a3e4e70843f7a34100f6470c04b791330788df"
readonly PACK_COLLECTION="nightly-251031"
readonly PACK_DB_COMMIT="00bf4076a730ed8b0a5c45e49dbb8318a3170127"
readonly PACK_COMMIT="cb842e66c7e9834fae9cdf05ba33ca64fbec949f"
readonly PACK_HOME="/home/vscode"
readonly PACK_STATE_DIR="${PACK_HOME}/.local/state/pack"
readonly PACK_CACHE_DIR="${PACK_HOME}/.cache/pack"
readonly PACK_CONFIG_DIR="${PACK_HOME}/.config/pack"
readonly PACK_BIN_DIR="${PACK_HOME}/.local/bin"
readonly IDRIS2_PREFIX="${PACK_STATE_DIR}/install/${IDRIS2_COMMIT}/idris2"
readonly IDRIS2_BOOT="${IDRIS2_PREFIX}/bin/idris2"
readonly BUILD_ROOT="/tmp/iotatime-idris-toolchain"

clone_at() {
  local url="$1"
  local commit="$2"
  local destination="$3"

  git init --quiet "$destination"
  git -C "$destination" remote add origin "$url"
  git -C "$destination" fetch --quiet --depth 1 origin "$commit"
  git -C "$destination" checkout --quiet FETCH_HEAD
}

install_package() {
  local name="$1"
  local url="$2"
  local commit="$3"
  local ipkg="$4"
  local source_dir="${BUILD_ROOT}/${name}"

  clone_at "$url" "$commit" "$source_dir"
  (cd "$source_dir" && "$IDRIS2_BOOT" --install-with-src "$ipkg")
}

rm -rf "$BUILD_ROOT"
mkdir -p "$BUILD_ROOT" "$PACK_BIN_DIR" "$PACK_CONFIG_DIR" \
  "$PACK_STATE_DIR/db" "$PACK_STATE_DIR/install" "$PACK_CACHE_DIR/clones"

clone_at "https://github.com/idris-lang/Idris2.git" "$IDRIS2_COMMIT" "${BUILD_ROOT}/Idris2"
make -C "${BUILD_ROOT}/Idris2" bootstrap SCHEME=chezscheme PREFIX="$IDRIS2_PREFIX"
make -C "${BUILD_ROOT}/Idris2" install PREFIX="$IDRIS2_PREFIX"
make -C "${BUILD_ROOT}/Idris2" clean
make -C "${BUILD_ROOT}/Idris2" all \
  IDRIS2_BOOT="$IDRIS2_BOOT" PREFIX="$IDRIS2_PREFIX" IDRIS2_CG=chez
make -C "${BUILD_ROOT}/Idris2" install \
  IDRIS2_BOOT="$IDRIS2_BOOT" PREFIX="$IDRIS2_PREFIX" IDRIS2_CG=chez
make -C "${BUILD_ROOT}/Idris2" install-with-src-libs \
  IDRIS2_BOOT="$IDRIS2_BOOT" PREFIX="$IDRIS2_PREFIX" IDRIS2_CG=chez
make -C "${BUILD_ROOT}/Idris2" install-with-src-api \
  IDRIS2_BOOT="$IDRIS2_BOOT" PREFIX="$IDRIS2_PREFIX" IDRIS2_CG=chez

install_package algebra https://github.com/stefan-hoeck/idris2-algebra.git \
  829f44b7fd961e3f0a7ad9174b395f97ebc33336 algebra.ipkg
install_package ref1 https://github.com/stefan-hoeck/idris2-ref1.git \
  ef6d4265deaa6a4f1b5228932102847a4e54e4d2 ref1.ipkg
install_package array https://github.com/stefan-hoeck/idris2-array.git \
  cecbd1dd3bae94669a2ed3689ee91ce1616cc34f array.ipkg
install_package bytestring https://github.com/stefan-hoeck/idris2-bytestring.git \
  082c5114b4016425c9957e955e22fcb0b194ada4 bytestring.ipkg
install_package getopts https://github.com/idris-community/idris2-getopts.git \
  0d41b98f83f3707deb0ffbc595ef36b7d9cb9eab getopts.ipkg
install_package elab-util https://github.com/stefan-hoeck/idris2-elab-util.git \
  6786ac7ef9931b1c8321a83e007f36a66e139e86 elab-util.ipkg
install_package refined https://github.com/stefan-hoeck/idris2-refined.git \
  c585013c33ad5398c91beed71fec61a5b721a8da refined.ipkg
install_package literal https://github.com/stefan-hoeck/idris2-literal.git \
  f0fed86ae9bd5b13d98e4dd18103a6e1f7f7c6b4 literal.ipkg

clone_at https://github.com/stefan-hoeck/idris2-ilex.git \
  c2d5a219c701a8f694aa95e8d34c7a58d58e5795 "${BUILD_ROOT}/ilex"
(cd "${BUILD_ROOT}/ilex/core" && "$IDRIS2_BOOT" --install-with-src ilex-core.ipkg)
(cd "${BUILD_ROOT}/ilex" && "$IDRIS2_BOOT" --install-with-src ilex.ipkg)
(cd "${BUILD_ROOT}/ilex/toml" && "$IDRIS2_BOOT" --install-with-src ilex-toml.ipkg)

install_package filepath https://github.com/stefan-hoeck/idris2-filepath.git \
  0441eaee9ff1d921fc3f4619c2a8d542588c0e99 filepath.ipkg

clone_at https://github.com/stefan-hoeck/idris2-pack.git "$PACK_COMMIT" "${BUILD_ROOT}/pack"
(cd "${BUILD_ROOT}/pack" && "$IDRIS2_BOOT" --build pack.ipkg)
cp -R "${BUILD_ROOT}/pack/build/exec/." "$PACK_BIN_DIR/"

install_package lsp-lib https://github.com/idris-community/lsp-lib.git \
  ca77e80a392b8cfeee3aaeb150069957699cdb82 lsp-lib.ipkg
clone_at https://github.com/idris-community/idris2-lsp.git \
  81344545c134c8e7105ecf1fdd7a1caae6647035 "${BUILD_ROOT}/idris2-lsp"
(cd "${BUILD_ROOT}/idris2-lsp" && "$IDRIS2_BOOT" --build idris2-lsp.ipkg)
cp -R "${BUILD_ROOT}/idris2-lsp/build/exec/." "$PACK_BIN_DIR/"

curl --fail --location --silent --show-error \
  "https://raw.githubusercontent.com/stefan-hoeck/idris2-pack-db/${PACK_DB_COMMIT}/collections/${PACK_COLLECTION}.toml" \
  --output "${PACK_STATE_DIR}/db/${PACK_COLLECTION}.toml"

cat >"${PACK_STATE_DIR}/pack.toml" <<EOF
collection = "${PACK_COLLECTION}"
EOF

cat >"${PACK_CONFIG_DIR}/pack.toml" <<'EOF'
[install]
with-src = true
whitelist = [ "pack", "idris2-lsp" ]

[idris2]
scheme = "chezscheme"
codegen = "chez"
bootstrap = true
bootstrap-stage3 = true
repl.rlwrap = true
EOF

ln -s "$IDRIS2_BOOT" "${PACK_BIN_DIR}/idris2"

chown -R vscode:vscode "${PACK_HOME}/.local" "${PACK_HOME}/.cache" "${PACK_HOME}/.config"
rm -rf "$BUILD_ROOT"

sudo -u vscode env PATH="$PATH" "${PACK_BIN_DIR}/idris2" --version
sudo -u vscode env PATH="$PATH" "${PACK_BIN_DIR}/pack" info
sudo -u vscode env PATH="$PATH" "${PACK_BIN_DIR}/idris2-lsp" --version
