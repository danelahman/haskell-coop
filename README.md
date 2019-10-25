# Haskell-Coop

Haskell-Coop is an experimental Haskell library for programming with effectful runners. It is based on
ongoing research of [Danel Ahman](https://danel.ahman.ee) and [Andrej Bauer](http://www.andrej.com).

Until a proper publication about this research has appeared, you might want to check the
talk [Interacting with external resources using runners (aka comodels)](https://danel.ahman.ee/talks/chocola19.pdf)
for an overview of effectful runners and how we transform them into a programming language construct.
For general background reading on algebraic effects and handlers, we recommend the lecture
notes [What is algebraic about algebraic effects and handlers?](https://arxiv.org/abs/1807.05923).
Section 4 of these lecture notes discusses ordinary runners of algebraic effects (also known in the
literature as comodels of algebraic effects).

## Prerequisites

To build Haskell-Coop, you need a working installation
of [Haskell](https://www.haskell.org/platform/), 
and the [Cabal](https://www.haskell.org/cabal/) package manager and build system.

## Building haskell-coop

You can type:

- `make` to locally build the Haskell-Coop library, generate documentation, and typecheck examples.
- `make build` to locally build the Haskell-Coop library and generate documentation.
- `make examples` to typecheck examples.
- `make clean` to clean up.

Building Haskell-Coop uses `cabal new-build` to locally build the library and all its dependencies,
and to generate Haddock documentation. The generated documentation can be found in
`dist-newstyle/build/platform/ghc-version/haskell-coop-version/doc/html/haskell-coop/index.html`.
The examples are typechecked with `ghci`.

## Entry points

There are two main entry points to the Haskell-Coop library:

- [src/Control/Runner.hs](src/Control/Runner.hs) that implements a restricted form of effectful runners (without exceptions and signals). 
- [src/Control/SignalRunner.hs](src/Control/SignalRunner.hs) that implements the general form of effectful runners (with exceptions and signals).

The directories [src/Control/Runner/](src/Control/Runner/) 
and [src/Control/SignalRunner/](src/Control/SignalRunner/) contain various example 
runners that implement file IO, ML-style state, their combinations, ambient functions as present 
in the [Koka](https://github.com/koka-lang/koka) language, etc.

Example uses of these runners can be found in [examples/without_signals/](examples/without_signals/) 
and [examples/with_signals/](examples/with_signals/).

## Further documentation

Further documentation of the Haskell-Coop library can be found in its individual modules.

## Acknowledgements

<table>
      <tr><td>This project has received funding from the European Union’s Horizon 2020 research and innovation programme under the Marie Skłodowska-Curie grant agreement No 834146.</td><td><img src="https://danel.ahman.ee/images/eu_flag.jpg"></td></tr>
      <tr><td>This material is based upon work supported by the Air Force Office of Scientific Research under award number FA9550-17-1-0326.</td><td></td></tr>
</table>
