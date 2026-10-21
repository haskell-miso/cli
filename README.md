# 🔋 miso

The command line tool for [miso](https://haskell-miso.org) apps: an
executable with the [MicroHs](https://github.com/augustss/MicroHs) compiler
and [MicroCabal](https://github.com/augustss/MicroCabal) inside, and miso
and its dependencies already installed.

```sh
curl -fsSL https://haskell-miso.org/download | sh
```

It downloads the release for your OS and architecture and checks its
sha256.  Then it shows what it will create, replace and remove in `~/.miso`
(or `$MISO_HOME`) and asks before installing; it lists what it installed,
asks to add `~/.miso/bin` to `PATH` in your shell's startup file
(`.zshrc`, `.bashrc` or `.bash_profile`, fish's `config.fish`, or
`.profile`), and checks the installation with `miso doctor` (which says
when emcc, for the browser, is missing).  `sh -s -- --yes` does not ask, and
`--no-modify-path` leaves the startup files alone; `MISO_VERSION=v0.1.0`
installs that release.  On NixOS it says to install miso with nix
instead, and installs nothing (`--yes` installs in `~/.miso` anyway):

```sh
nix profile install github:haskell-miso/cli#release   # the release, prebuilt
nix profile install github:haskell-miso/cli           # built from source
```

`#release` is the tarball of the latest GitHub release (the one `install.sh`
downloads), pinned with its sha256 in `nix/release.json`, which the release
workflow updates on main after each release.

```sh
miso new counter      # a new project: miso.yml, app/Main.hs, static/index.html
cd counter
miso build            # public/index.js (target: browser)
```

## Commands

| command | |
|---|---|
| `miso new NAME [--template T]` | a new project; `miso templates` lists the templates (counter, router, canvas) |
| `miso init` | make the current directory a project. On a terminal it asks for the name (default: the directory's), version, synopsis, author (from git), license and template, one at a time; `--name`, `--version`, `--synopsis`, `--author`, `--license` and `--template` answer them, and `--yes` takes the defaults. It writes the template's files that are not there yet (a `.gitignore` gets the lines it lacks), and a `LICENSE` for BSD-3-Clause, BSD-2-Clause and MIT |
| `miso build [NAME...]` | install missing dependencies, and build the project of `miso.yml` (or the programs named). On a terminal it shows the last lines of the build as it runs, as `miso dev` does, and the log (`.miso-build.log`) when it fails |
| `miso run [NAME] [ARGS]` | build and run a program (`target: native`; the first one, or NAME) |
| `miso test [NAME...] [-- ARGS]` | build the tests of `miso.yml` (or those named) and run each, with ARGS; it fails when a test does |
| `miso dev [NAME] [--port N]` | build a browser program (the first one, or NAME), serve `public/`, and on a change of the sources rebuild and reload the page |
| `miso serve [--port N]` | serve `public/` (on the first free port from 8000, or N) |
| `miso watch [NAME]` | type check the library, the programs and the tests (or NAME) on every change, and show the first error, as ghcid does |
| `miso lint [ARGS]` | run [hlint](https://github.com/ndmitchell/hlint) on the source dirs of `miso.yml`, with ARGS; it reads `.hlint.yaml` |
| `miso repl` | mhs, interactively, with the project's sources and packages |
| `miso clean` | remove `dist/`, `public/`, `.miso/`, and the caches |
| `miso install` | install the dependencies of `miso.yml`, in the project's `.miso/` |
| `miso add PKG...` / `miso remove PKG...` | edit the dependencies of `miso.yml` (keeping its comments), and install |
| `miso list` | the packages: those that come with miso, and in a project the ones it installed in `.miso/` |
| `miso lock` | write `miso.lock`: the dependencies of `miso.yml` and the packages they need; lock what is not locked yet, keep what is |
| `miso update` | a new package list for mcabal, and `miso.lock` again at its versions (git dependencies at their current commits); then install |
| `miso vendor` | lock as `miso lock` does, and copy the source of each locked dependency to `vendor/NAME`; builds then install from there, and fail rather than download |
| `miso doctor` | check the installation and the tools miso needs, and say how to fix what is missing |
| `miso upgrade [VERSION]` | install the latest release (or `VERSION`) in place |
| `miso uninstall [--yes]` | remove miso: `$MISO_HOME` (packages, caches) and the tool where `install.sh` put it. It shows what it removes and asks first; it lists the lines of your shell files that add miso to `PATH` (it does not edit them) |
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
synopsis: A counter        # optional: one line about the package
author: Ada <ada@example.org>   # optional
license: BSD-3-Clause      # optional: an SPDX id
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
    path: ../my-lib        # (installed again when its files change)
  - name: aeson            # a version from Hackage, not the package list's
    version: 2.2.3.0
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

Tests are programs that run on this machine, and pass when they exit with 0
(as Cabal's `exitcode-stdio` tests):

```yaml
tests:
  spec:                    # miso test spec
    main: Spec
    source-dirs: [test]    # default [test]
    dependencies: [split]  # added to the project's
```

- `miso test` builds every test to `dist/NAME`, runs each (with the
  arguments after `--`), and fails when one of them fails; all of them run.
  `miso build` does not build the tests.
- A test can import the library's modules, has no `target` or `static-dir`,
  and takes `mhs-options` and the fields below, as a program does.  A test
  and a program cannot have the same name.

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

## Packages

Some packages come with the tool, in its package database
(`~/.miso/mhs-VERSION`): the compiler's (`base`, `ghc-compat`, `array-mhs`,
`template-haskell`), and the libraries `containers`, `mtl`, `transformers`
and `miso`.  The packages a project installs go in the project, in
`.miso/` (next to `miso.yml`; `miso new` puts it in `.gitignore`, and
`miso clean` removes it).  mhs looks for packages in the project's
`.miso/` first, then in the tool's database, and nowhere else: what one
project installs is not seen by another, so two projects can use two
versions of a package.

Building a package takes time, so each one is kept in a store,
`~/.miso/store/mhs-VERSION/`, and a project that needs the same one again
copies it from there (`Installing split 0.2.5.1 (built already, from the
store)`).  A package is the same one when it has the same source (the sha256
of a Hackage tarball, or a git commit), cabal flags and compiler, and was
built against the same packages; one built against a local (`path:`)
package is not kept (it is built again when that changes).  The downloads
and git checkouts are kept in `~/.miso/src`, and mcabal's package list in
`~/.miso/mcabal`, for every project.  A project with no `miso.lock` gets its
dependencies locked for the install, as `miso lock` would (without writing
`miso.lock`), so they are kept in the store too.

A project can replace a library that comes with the tool: a dependency on
it with `git:`, `path:`, or a `version:` that is not the tool's.  It is
installed in the project's `.miso/`, and the tool's libraries that need it
are built again in the project, from their source (the release ships the
source of its libraries, in `~/.miso/mhs-VERSION/miso-sources`), against
it; `miso list` shows which:

```yaml
dependencies:
  - name: transformers     # instead of the tool's 0.6.1.2: mtl and miso,
    version: 0.6.1.1       # which need it, are built again against it
  - name: miso             # miso from a checkout (with the flags the tool's
    path: ../miso          # is built with, in miso-sources/miso-*.flags)
    flags: -template-haskell -aeson -native -production -ssr -benchmark -text
```

The compiler's packages cannot be replaced (`miso build` says so).
`miso.lock` records the versions of the tool's libraries a project uses; a
newer tool may come with others, and `miso build` then warns (`miso lock`
records them; to keep one, give its `version:` in `miso.yml`).

## miso.lock

`miso lock` and `miso update` write `miso.lock`, next to `miso.yml`; commit
it.  It has the dependencies of `miso.yml` and every package they need, in
the order they install in (each after the packages it needs): for a package
from Hackage, a version and the sha256 of its tarball, and for a git
dependency the commit its `rev` (a tag, a commit, or a branch) is at, and the
version of the package there (from its `miso.yml`, or its `.cabal` file):

```yaml
# miso.lock: written by miso lock and miso update.  Commit it; do not edit it.
# Local (path:) dependencies are not locked.
miso: 0.1.0
mhs: 0.16.10.0
bundled: containers-0.8 miso-1.14.0.0 mtl-2.3.1 transformers-0.6.1.2
packages:
  - name: split
    hackage: 0.2.5.1
    sha256: 18adb60f7f12d08601c382d47c2bacbbd4de4b9233f09683889d15b64b6420d6
  - name: data-default         # needed by data-default-class
    hackage: 0.8.0.2
    sha256: fa7d5de70ccef7381a7f17cd64948de1c24b620de13e1cd55c36c26949870799
  - name: data-default-class
    hackage: 0.2.0.0
    sha256: 0ae530c4fb0bd6c8a8ba399429ccd9c75f9c7696049117178f4ceeb2bd08d5b4
  - name: lens-family
    git: https://github.com/example/lens-family
    rev: v2.1.3
    commit: 8fa9e00cca7db83a3270e12943f1820dad1f2472
    version: 2.1.3
```

- The packages a package needs are the `build-depends` of its library and
  executables, as mcabal reads them: for this OS and architecture, with the
  dependency's `flags`, and with MicroHs's names (`random` is
  `random-mhs`).  Their versions are those of mcabal's package list (a
  Stackage snapshot, from `miso update`), as mcabal would install them; a
  package that is not in it gets Hackage's newest version, and `miso lock`
  says so.  The packages that come with the tool, and those built into
  MicroHs (`base`, `text`, `bytestring`, ...), are not locked.
- `miso lock` locks what is not in `miso.lock` yet (a dependency added to
  `miso.yml`, one whose `git`/`rev` changed, or a package a new one needs),
  keeps the packages that still match at their versions, and drops those no
  longer needed.
- `miso update` gets mcabal a new package list (the newest Stackage
  snapshot), locks every package again at its version, and git
  dependencies at the current commit of their `rev`, and installs them.
- A git dependency needs a `rev:` (a tag, or a commit; a branch works but
  moves).  When the `rev` is a version tag (`v2.1.3` or `2.1.3`) that is not
  the version in the package's description, `miso lock` and `miso update`
  warn.
- `miso.lock` is resolved on one OS: a package that only another OS needs
  (`if os(windows)`, say) is not in it.

`miso build` and `miso install` then install exactly that, each package
after the ones it needs: the Hackage tarball of each version (checked against
its sha256), and the git commit, until the next `miso lock` or `miso update`;
mcabal installs nothing a locked package needs by itself.  Local (`path:`)
dependencies are not locked (they are installed from the directory when its
files change), and neither are the packages that come with the tool, unless
the project replaces one (see Packages).  A dependency that is not in
`miso.lock` (added to `miso.yml` since, or with a different `git`/`rev`) is
locked for the install, as without a `miso.lock`, with a warning.

## vendor/

`miso vendor` runs `miso lock`, then copies the source of each package in
`miso.lock` to `vendor/NAME`: the Hackage tarball (checked against its
sha256), unpacked, or the files of the git commit (`git archive`, so no
`.git`).  `vendor/.miso-vendor` lists them, each with its version or commit
and a sha256 of its files:

```
# vendor/.miso-vendor: written by miso vendor.  Commit it; do not edit it.
hackage split 0.2.5.1 18adb60f... files:9f0560fc...
```

Commit `vendor/` with `miso.lock`.  A project with `vendor/.miso-vendor` is
vendored:

- `miso build` and `miso install` install the locked dependencies from
  `vendor/` (or copy them from the store, when it has the same package
  built), and never download them.  A dependency that is not locked, not
  in `vendor/`, at another version than `miso.lock`'s, or whose files have
  changed since `miso vendor` copied them, is an error: run `miso vendor`
  (after `miso update`, too).
- `miso vendor` keeps a package that is there as it copied it, copies again
  one that changed, and removes one that is no longer a dependency.  Other
  files in `vendor/` are left alone.  `vendor/.gitattributes` keeps git from
  changing line endings (which would change the sha256), and
  `vendor/.gitignore` ignores what mcabal builds there.
- Do not edit the vendored sources; for a patched copy, use a `path:`
  dependency.
- Every package of `miso.lock` is vendored: the dependencies of `miso.yml`
  and the packages they need.  Local dependencies and the packages that come
  with the tool are not.
  Removing `vendor/` makes the project an ordinary one again.

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

Pushing a `v*` tag builds five platforms (x86_64 and aarch64 Linux and
aarch64 macOS with nix; Intel macOS and Windows, which the miso flake's
nixpkgs does not support, with `scripts/build-release.sh`) and attaches them
to the release, where `install.sh` finds them.
`https://haskell-miso.org/download` serves a copy of `install.sh`: the
website's `static/download`, in
[haskell-miso.org](https://github.com/haskell-miso/haskell-miso.org).  A
change to `install.sh` here goes there with a pull request to that
repository.  `miso upgrade` downloads `install.sh` from
`https://raw.githubusercontent.com/haskell-miso/cli/main/install.sh` (or
`MISO_INSTALL_URL`).  The
release assets must be downloadable without a GitHub login, so the releases
need a public repository.

## Phase 2

- `miso dev`: a web server (in the binary) that serves `public/`, rebuilds
  on changes to the sources, and tells the page to reload (server sent
  events).
