# miso

The command line tool for [miso](https://haskell-miso.org) apps: an
executable with the [MicroHs](https://github.com/augustss/MicroHs) compiler
and [MicroCabal](https://github.com/augustss/MicroCabal) inside, and miso
and its dependencies already installed.

```sh
curl -fsSL https://haskell-miso.org/install.sh | sh
```

```sh
miso new counter      # a new project: miso.yml, app/Main.hs, static/index.html
cd counter
miso build            # public/index.js (target: browser)
```

## Commands

| command | |
|---|---|
| `miso new NAME` | make a new project |
| `miso build` | install missing dependencies, and build the project of `miso.yml` |
| `miso run [ARGS]` | build and run it (`target: native`) |
| `miso install` | install the dependencies of `miso.yml` |
| `miso dev` | serve the app and reload it on changes (phase 2, not done yet) |
| `miso mhs ARGS` | the MicroHs compiler |
| `miso mcabal ARGS` | MicroCabal |

Output is colored on a terminal; set `NO_COLOR=1` to turn that off.

## miso.yml

```yaml
name: counter
main: Main                 # the main module (default Main)
source-dirs: [app]         # default [app]
target: browser            # browser (WebAssembly, needs emcc), browser_js, or native
mhs-options: []            # passed to mhs
dependencies:
  - split                  # from Hackage (Stackage), with mcabal
  - name: lens-family      # from git, at a revision, with cabal flags
    git: https://github.com/example/lens-family
    rev: v2.1.3
    flags: -foo +bar
  - name: my-lib           # a local checkout, relative to miso.yml
    path: ../my-lib        # (installed again on every build)
```

base, containers, mtl, transformers, array, and miso come with the tool.
Other packages are installed next to them, in `~/.miso/mhs-VERSION`
(`install.sh` unpacks the tool in `$MISO_HOME`, default `~/.miso`); a new
release starts with a fresh package database, and `miso build` installs them
again.

## Requirements

- A C compiler (`cc`): mhs compiles programs to C.
- For `target: browser`, [emscripten](https://emscripten.org) (`emcc`).
- `curl`, `tar`, and `git` (for git dependencies).

## How it is built

`src/Miso/CLI/Main.hs` imports `MicroHs.Main` and `MicroCabal.Main`, so the
whole tool is one Haskell program.  mhs compiles it to one C file, which is
linked with the MicroHs runtime: statically on Linux (musl); on macOS only
libSystem is dynamic, as for every macOS program.  `miso mcabal` runs
MicroCabal with `MHS` set to `miso mhs`, so packages are compiled by the same
compiler.

The release tarball is laid out as an mhs installation:

```
miso-aarch64-darwin/
  bin/miso, bin/cpphs (for modules with CPP)
  mhs-0.16.7.0/        the package database: base, miso, ..., and the runtime sources
```

MicroHs and the package database with miso come from the
[miso flake](https://github.com/dmjio/miso) (`pkgs.microhs`,
`pkgs.miso-mhs`), MicroCabal from its repository.  MicroCabal's
`Text.ParserComb` clashes with MicroHs's, so `scripts/prepare-microcabal.sh`
renames it (and switches its downloads from wget to curl).

```sh
nix build .#default        # result/bin/miso
nix build .#tarball        # result/miso-SYSTEM.tar.gz, as released

# or with an installed mhs and the two source trees
make MHSSRC=../MicroHs MCABALSRC=../MicroCabal
```

Pushing a `v*` tag builds three platforms (x86_64 and aarch64 Linux,
aarch64 macOS; the miso flake's nixpkgs no longer supports Intel Macs, which
can build with `make` and an installed mhs) and attaches them to the release, where `install.sh` finds them.
`install.sh` is to be served at `https://haskell-miso.org/install.sh`.  The
release assets must be downloadable without a GitHub login, so the releases
need a public repository.

## Phase 2

- `miso dev`: a web server (in the binary) that serves `public/`, rebuilds
  on changes to the sources, and tells the page to reload (server sent
  events).
