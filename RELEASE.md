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
5. Review public API and README changes, then create the release tag.

## Scope boundary

Date-bearing patterns are Gregorian-specific in version 0.1.0. Generalizing pattern fields across calendars is an optional major extension, not a release blocker for the documented API.