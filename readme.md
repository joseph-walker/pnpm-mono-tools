# Mono-Tools

Opinionated tooling for managing dependencies in a PNPM / Turbo Monorepo

## Installing

### Binary

For now, find latest build in Artifacts section of latest Action run [here](https://github.com/joseph-walker/pnpm-mono-tools/actions).

TODO:
- Builds for different OS/Arch (Currently `x86_64-linux` only)
- Upload build artifacts to the "Releases" channel for the repo w/ proper changelogs

### Development

Built with GHC v9.2.4 & Built with __GHC v9.2.4__ & __Cabal v3.6.2.0__

The easiest way to setup for development is with [GHCup](https://www.haskell.org/ghcup/)

1) Ensure that GHC and Cabal are both installed
2) Clone this repo
3.a) `cabal run` inside of the cloned directory to run the app
3.b) `cabal build` to create the executable

The build artifact will be placed in a different output depending on the architecture of your machine and the
version of GHC you use.

E.g. on M1:

```
./dist-newstyle/build/aarch64-osx/ghc-9.2.4/mono-tools-0.1.0.0/x/mono-tools/build/mono-tools/mono-tools
```
