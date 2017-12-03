# Description

Game Master's Companion

A set of APIs that assist a game master during a table top game session.  The
APIs allow a game master or players to manipulate characters, equipment, and
other normal game elements.

# Getting Started

The API is available at <https://api.dungeon.studio>.

Developer documentation can be generated with:

```bash
cabal haddock --executables
```

Once the documentation is generated, it is available at:
`./dist/doc/html/api-dungeon-studio/api-dungeon-studio/index.html`.

## Locally with [`docker-compose`][docker-compose]

This project is setup to run with [`docker-compose`][docker-compose].  Running
the following command will build a [docker] image (includes building
api-dungeon-studio), and start all requisite services as [docker] containers.

```bash
docker-compose up -d
```

api-dungeon-studio will be available at <http://localhost:45753> once this
command finishes executing.

## Locally with [`nix-shell`][nix-shell]

This project is setup with [`nix-shell`][nix-shell].  Running the following
command will build a local development environment where all of the
supplementary tools are pre-installed.

```bash
nix-shell
```

Once this command finishes executing, [`cabal`][cabal] and other tools are
available.

## Others

This project utilizes [`cabal`][cabal] like most [Haskell] projects and the
standard [Haskell] development environment for your platform should work just
fine.

# Reporting Issues

Any issues discovered should be recorded on [github][issues].  If you believe
you've found an error or have a suggestion for a new feature; please, ensure
that it is reported.

If you would like to contribute a fix or new feature; please, submit a pull
request.  This project follows [git flow] and utilizes [travis] to automatically
check pull requests before a manual review.

# Contributors

The `COPYRIGHT` file contains a list of contributors with their respective
copyrights and other information.  If you submit a pull request and would like
attribution; please, add yourself to the `COPYRIGHT` file.

I suggest the following script for validating any changes you might submit:

```bash
#!/usr/bin/env bash

set -e

hlint .

cabal clean

cabal configure -O0 --enable-tests --enable-benchmarks
cabal build -j --ghc-options="-Werror"
cabal test  -j --ghc-options="-Werror" --show-details=always

cabal haddock
```

[cabal]: https://www.haskell.org/cabal/
[docker-compose]: https://docs.docker.com/compose/
[docker]: https://docs.docker.com/
[git flow]: http://nvie.com/posts/a-successful-git-branching-model/
[Haskell]: https://www.haskell.org/
[issues]: https://github.com/alunduil/api.dungeon.studio/issues
[nix-shell]: https://nixos.org/nix/manual/#sec-nix-shell
[travis]: https://travis-ci.org/alunduil/api.dungeon.studio
