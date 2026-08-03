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

Canonical and numeric date-bearing patterns support every shipped calendar through `CalendarPattern`. Operating-system locale date layouts remain Gregorian because native locale snapshots provide exactly 12 Gregorian month names.