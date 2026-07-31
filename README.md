# iotaTime

An Idris 2 time library based on Erik Naggum's "Long painful history of time".

## Development container / Codespaces

This repository is configured for GitHub Codespaces and VS Code Dev Containers via `.devcontainer/`.

- Uses an official Microsoft devcontainer base image (`mcr.microsoft.com/devcontainers/base:ubuntu-24.04`)
- Builds and installs Idris 2 from the official `idris-lang/Idris2` source release tag
- Installs common container utilities and recommended VS Code extensions

## Project layout

- `iotaTime.ipkg` — Idris 2 library package definition
- `src/IotaTime.idr` — library entry module
- `test/iotaTime-test.ipkg` — test package
- `test/Main.idr` — basic compile/runtime smoke test

## Build and test

```bash
idris2 --build iotaTime.ipkg
idris2 --build test/iotaTime-test.ipkg
./build/exec/iotaTime-test
```
