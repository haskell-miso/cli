# 🔋 miso

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
| `miso new NAME [--template T]` | a new project; `miso templates` lists the templates (counter, router, canvas) |
| `miso build [NAME...]` | install missing dependencies, and build the project of `miso.yml` (or the programs named) |
| `miso run [NAME] [ARGS]` | build and run a program (`target: native`; the first one, or NAME) |
| `miso dev [NAME] [--port N]` | build a browser program (the first one, or NAME), serve `public/`, and on a change of the sources rebuild and reload the page |
| `miso serve [--port N]` | serve `public/` (on the first free port from 8000, or N) |
| `miso watch [NAME]` | type check the library and the programs (or NAME) on every change, and show the first error, as ghcid does |
| `miso repl` | mhs, interactively, with the project's sources and packages |
| `miso clean` | remove `dist/`, `public/`, and the caches |
| `miso install` | install the dependencies of `miso.yml` |
| `miso add PKG...` / `miso remove PKG...` | edit the dependencies of `miso.yml` (keeping its comments), and install |
| `miso list` | the installed packages: those that come with miso, and those you installed |
| `miso update` | lock the Hackage and git dependencies in `miso.lock` (newest versions, current commits), and install them |
| `miso doctor` | check the installation and the tools miso needs, and say how to fix what is missing |
| `miso upgrade [VERSION]` | install the latest release (or `VERSION`) in place |
| `miso mhs ARGS` / `miso mcabal ARGS` | the MicroHs compiler, and MicroCabal |

Output is colored on a terminal; set `NO_COLOR=1` to turn that off.

With a MicroHs that has the browser runtime (target `browser_rts`), `miso
dev` does not run emcc on a save of a `target: browser` program: mhs writes
the program as combinators (`app.comb` next to `index.js` in `public/`, about
3s), and a runtime built once with emcc runs it.  The runtime is built for the
program's foreign imports and its C and JavaScript files (`c-sources`,
`js-sources`, include files) with their options, and kept in `$MISO_HOME/rts`
for later builds with the same ones; a new `foreign import`, or an edit to one
of those files, builds another one, once.

`miso dev` and `miso serve` use a small server in the binary
(`cbits/serve.c`), on 127.0.0.1.  In dev mode HTML pages get a script that
polls the server and reloads them when a build succeeds; a failed build
keeps the last one.

## miso.yml

```yaml
name: counter
version: 0.1.0             # required: numbers separated by dots
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

`main`, `source-dirs` and `target` there describe the project's one program.
A project with a library, or with several programs, has sections for them
instead:

```yaml
name: todo
version: 0.1.0
dependencies: [split]      # for every component
library:                   # code the programs share
  source-dirs: [src]       # default [src]
  exposed-modules: [Todo]  # default: every module under source-dirs
executables:
  web:                     # miso build web, miso dev web, ...
    main: Main
    source-dirs: [app]
    target: browser
  cli:
    main: Cli
    source-dirs: [cli]
    dependencies: [containers]   # added to the project's
```

- Every program can import the library's modules (mhs gets its
  `source-dirs` first).  The library and each program may also have
  `dependencies` and `mhs-options`, added to the project's.
- `miso build` type checks the library (`mhs -fno-code`) and builds every
  program: native ones to `dist/NAME`, a browser one to `public/`, or with
  several browser programs, each to `public/NAME/`.
- The top-level `main`, `source-dirs` and `target` (and the fields below)
  cannot be used with `executables:`.  A project with just `library:` is a library: `miso build`
  checks it, and there is nothing to run.

The library, each program, and a project with one program also take:

```yaml
c-sources: [cbits/*.c]       # C files (globs; * and ? in the last part)
js-sources: [jsbits/*.js]    # JavaScript for browser programs (emcc --pre-js)
include-dirs: [include]      # -I for the C files and for CPP
cpp-options: [-DDEBUG]       # for modules with {-# LANGUAGE CPP #-}
extra-libraries: [m]         # -lm for the linker
cc-options: [-DSCALE=2]      # for the C compiler
ld-options: []               # for the linker
static-dir: static           # programs only: copied next to a browser program
```

Without `c-sources`, `js-sources` or `include-dirs`, `cbits/*.c`,
`jsbits/*.js` and `include/` are used when they are there: the library's,
or in a project without one, each program's.  What a library has goes into
every program.  Browser programs are built with WebAssembly's current
exception instructions (`-sWASM_LEGACY_EXCEPTIONS=0`), which browsers keep;
the old `try` instruction is deprecated.

`miso watch` runs `mhs -fno-code` (under a second, where a build with emcc
takes many) on the library and each program, clears the screen, and shows
`All good` or the first error as `file:line:col: message`.  mhs stops at the
first error, so after fixing one, the next shows on the next save.

base, containers, mtl, transformers, array, and miso come with the tool.
Other packages are installed next to them, in `~/.miso/mhs-VERSION`
(`install.sh` unpacks the tool in `$MISO_HOME`, default `~/.miso`); a new
release starts with a fresh package database, and `miso build` installs them
again.

## miso.lock

`miso update` writes `miso.lock`, next to `miso.yml`; commit it.  It has, for
each dependency from Hackage, its newest version and the sha256 of its
tarball, and for each git dependency the commit its `rev` (a branch, a tag,
or a commit; HEAD without one) is at:

```yaml
# miso.lock: written by miso update.  Commit it; do not edit it.
# Local (path:) dependencies are not locked.
miso: 0.1.0
mhs: 0.16.10.0
packages:
  - name: split
    hackage: 0.2.5.1
    sha256: 18adb60f7f12d08601c382d47c2bacbbd4de4b9233f09683889d15b64b6420d6
  - name: lens-family
    git: https://github.com/example/lens-family
    rev: v2.1.3
    commit: 8fa9e00cca7db83a3270e12943f1820dad1f2472
```

`miso build` and `miso install` then install exactly that: the Hackage
tarball of that version (checked against its sha256), and the git commit,
until the next `miso update`.  Local (`path:`) dependencies are not locked
(they are installed from the directory on every build), and neither are the
packages that come with the tool.  A dependency that is not in `miso.lock`
(added to `miso.yml` since, or with a different `git`/`rev`) is installed as
without a lock, with a warning; without a `miso.lock` nothing changes.

Only the dependencies listed in `miso.yml` are locked: the packages they
depend on are still installed by mcabal, from its package list.

## Requirements

On Windows miso runs in [MSYS2](https://www.msys2.org) (an MSYS2 shell:
`pacman -S gcc make tar curl git`, then the install command above).

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
  mhs-0.16.10.0/       the package database: base, miso, ..., and the runtime sources
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
