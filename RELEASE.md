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
   git diff --check
   ```

4. Confirm Linux full-suite and Windows registry jobs pass in CI.
5. Review public API and README changes, then push a tag matching the package
   version, for example `v0.1.0`.

The tag workflow waits for the full Linux and Windows matrix, verifies the tag
against both version declarations, creates source and API-documentation
archives, publishes a GitHub Release, and submits the immutable release commit
to `idris2-pack-db`.

## One-time publishing setup

1. In repository **Settings > Actions > General > Workflow permissions**,
   select **Read and write permissions**.
2. Open or update an owner-authored pull request once so the documentation
   workflow creates the `gh-pages` branch. Then, in repository
   **Settings > Pages**, select **Deploy from a branch**, `gh-pages`, and
   `/ (root)`. Previews are published at
   `https://jason-johnson.github.io/iotaTime/pr-preview/pr-N/`.
3. Fork `stefan-hoeck/idris2-pack-db` to
   `jason-johnson/idris2-pack-db` on GitHub.
4. Create a classic personal access token with the `public_repo` scope. Add it
   to this repository under **Settings > Secrets and variables > Actions** as
   `PACK_DB_TOKEN`. GitHub currently requires that cross-repository scope for
   the workflow to push to your fork and open a pull request against the
   upstream public repository.

There is no Idris package-registry login or upload token. The extra credential
is a GitHub token used only to push a branch to the pack-db fork and open its
upstream pull request. GitHub's built-in `GITHUB_TOKEN` publishes the release
and PR documentation without any additional secret.

## Scope boundary

Canonical and numeric date-bearing patterns support every shipped calendar through `CalendarPattern`. Operating-system locale date layouts remain Gregorian because native locale snapshots provide exactly 12 Gregorian month names.