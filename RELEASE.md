# Release checklist

1. Update `version` in `iotaTime.ipkg` and `PACKAGE_VERSION` in `Makefile` together.
2. Run a clean library build and install:

   ```bash
   make clean-support
   idris2 --build iotaTime.ipkg
   idris2 --install iotaTime.ipkg
   idris2 --clean iotaTime.ipkg
   ```

3. Build installed-package consumers and run all checks:

   ```bash
   idris2 --build test/iotaTime-test.ipkg
   ./test/build/exec/iotaTime-test
   sh test/run-compile-fail-tests.sh
   idris2 --build examples/iotaTime-examples.ipkg
   ./examples/build/exec/iotaTime-zoned-meeting
   npm ci --prefix docs
   idris2 --mkdoc iotaTime.ipkg
   node docs/enhance-docs.mjs build/docs
   git diff --check
   ```

4. Confirm Linux full-suite and Windows registry jobs pass in CI.
5. Review public API and README changes, then push a tag matching the package
   version, for example `v0.1.0`.

The tag workflow waits for the full Linux and Windows matrix, verifies the tag
against both version declarations, creates source and API-documentation
archives, publishes a GitHub Release, and submits the immutable release commit
to a branch in the `idris2-pack-db` fork. Open the upstream pull request from
the URL in the workflow summary.

## One-time publishing setup

1. In repository **Settings > Actions > General > Workflow permissions**,
   select **Read and write permissions**.
2. Open a pull request from one of your branches in this repository. Wait for
   its **Documentation preview** workflow to finish; the workflow creates and
   manages the `gh-pages` branch automatically. Do not edit that branch.
3. In repository **Settings > Pages**, select **Deploy from a branch**, then
   select `gh-pages` and `/ (root)`. Previews are published at
   `https://jason-johnson.github.io/iotaTime/pr-preview/pr-N/`.
4. Fork `stefan-hoeck/idris2-pack-db` to
   `jason-johnson/idris2-pack-db` on GitHub.
5. Create a fine-grained personal access token scoped only to
   `jason-johnson/idris2-pack-db`, with **Contents: Read and write**. Add it to
   this repository under **Settings > Secrets and variables > Actions** as
   `PACK_DB_TOKEN`.
6. After a tag workflow pushes the release branch, follow its summary URL to
   open the pull request against `stefan-hoeck/idris2-pack-db` manually.

There is no Idris package-registry login or upload token. The extra credential
is a GitHub token used only to push a branch to the pack-db fork. GitHub's
built-in `GITHUB_TOKEN` publishes the release and PR documentation without any
additional secret.

## Scope boundary

Canonical and numeric date-bearing patterns support every shipped calendar through `CalendarPattern`. Operating-system locale date layouts remain Gregorian because native locale snapshots provide exactly 12 Gregorian month names.