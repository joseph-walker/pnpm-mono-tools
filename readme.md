# Mono-Tools

Opinionated tooling for managing dependencies in a PNPM / Turbo Monorepo

## Installing

### Binary

For now, find latest build in Artifacts section of latest `Release` Action run [here](https://github.com/joseph-walker/pnpm-mono-tools/actions).

TODO:
- Upload build artifacts to the "Releases" channel for the repo w/ proper changelogs

### Development

Built with __GHC v9.2.4__ & __Cabal v3.6.2.0__

The easiest way to setup for development is with [GHCup](https://www.haskell.org/ghcup/)

1) Ensure that GHC and Cabal are both installed
2) Clone this repo
3) `cabal run mono-tools --` inside of the cloned directory to run the app
4) `cabal build` to create the executable
5) (Optional) `cabal install` to create a sym-linked executable in your $PATH
6) (Optional) `cabal haddock --haddock-executables --haddock-hyperlink-source` to generate documentation

The build artifact will be placed in a different output depending on the architecture of your machine and the
version of GHC you use.

E.g. on M1:

```
./dist-newstyle/build/aarch64-osx/ghc-9.2.4/mono-tools-0.1.0.0/x/mono-tools/build/mono-tools/mono-tools
```
