# The miso manual

A reference for every command of the `miso` tool: what it does, its
arguments, what it writes, and when it fails.  For the files a project has
(`miso.yml`, `miso.lock`, `vendor/`) and where packages come from, see the
[README](../README.md); this manual links to it rather than repeating it.

- [Getting started](#getting-started)
- [Conventions](#conventions)
- Project: [new](#miso-new), [templates](#miso-templates), [init](#miso-init),
  [build](#miso-build), [run](#miso-run), [test](#miso-test),
  [dev](#miso-dev), [serve](#miso-serve), [watch](#miso-watch),
  [lint](#miso-lint), [repl](#miso-repl), [docs](#miso-docs),
  [clean](#miso-clean)
- Dependencies: [install](#miso-install), [add](#miso-add),
  [remove](#miso-remove), [list](#miso-list), [lock](#miso-lock),
  [update](#miso-update), [vendor](#miso-vendor), [cabal](#miso-cabal)
- The tool: [doctor](#miso-doctor), [upgrade](#miso-upgrade),
  [uninstall](#miso-uninstall), [version](#miso-version),
  [help](#miso-help), [mhs](#miso-mhs), [mcabal](#miso-mcabal)
- [Environment variables](#environment-variables)
- [Files and directories](#files-and-directories)

## Getting started

```sh
curl -fsSL https://haskell-miso.org/download | sh   # install (see the README)
miso doctor                                         # check the tools
miso new counter                                    # a new project
cd counter
miso dev                                            # build, serve, reload on save
```

A new project builds for the browser (`target: browser`, WebAssembly), which
needs [emscripten](https://emscripten.org) (`emcc`).  Without it, change
`target:` in `miso.yml` to `native` and use `miso run`.

## Conventions

- Commands that work on a project (everything but `new`, `templates`,
  `init`, `serve`, `repl`, `list`, `doctor`, `upgrade`, `uninstall`,
  `version`, `help`, `mhs` and `mcabal`) read `miso.yml` in the current
  directory (not in a parent directory), and fail without one.
- Output: `==>` lines say what miso is doing or has done, `->` lines are
  steps of it; `warning:` and `error:` go to standard error.  Colors are
  used on a terminal; `NO_COLOR=1` turns them off.
- Exit status: 0 when the command worked, 1 when it failed (after an
  `error:` line).  `miso run` and `miso lint` exit with the status of the
  program or of hlint; Ctrl-C exits with 130.
- Commands that build install the dependencies of `miso.yml` first, when
  they are not installed yet (see [install](#miso-install)).
- A failed download says `error: could not download URL (check the network
  connection, or the proxy)`; a failed git command names the command.

---

## Project

### miso new

```
miso new NAME [--template T]
miso new --template T NAME
```

Makes a directory `NAME` with a new project from a template (default
`counter`; [`miso templates`](#miso-templates) lists them).  `NAME` is
the package's name too: a letter, then letters, digits and `-`.

| file | |
|---|---|
| `miso.yml` | `name: NAME`, `version: 0.1.0`, `main: Main`, `source-dirs: [app]`, `target: browser`, `dependencies: [miso]` |
| `app/Main.hs` | the template's program |
| `static/index.html` | the page that loads the program (copied to `public/` by builds) |
| `.gitignore` | `public/`, `dist/`, `.miso/`, `.mhscache`, `.miso-build.log` |
| `README.md` | a short README: how to build it |

Fails when `NAME` exists already, or the template does not (it then lists
the templates).

### miso templates

```
miso templates
```

Lists the templates: `counter` (the smallest miso app, the default),
`router` (pages with URLs, and links between them) and `canvas` (drawing on
a canvas, driven by the model).

### miso init

```
miso init [--name NAME] [--version V] [--synopsis S] [--author A]
          [--license L] [--template T] [--yes]
```

Makes the current directory a project, keeping the files that are there.

On a terminal it asks, one at a time, for each value the flags do not
give, showing the default:

| question | default | accepted |
|---|---|---|
| name | the directory's name | a letter, then letters, digits and `-` |
| version | `0.1.0` | numbers separated by dots |
| synopsis | none | anything |
| author | `git config user.name` `<user.email>` | anything |
| license | `BSD-3-Clause` | an SPDX id, or `none` |
| template | `counter` | `counter`, `router`, `canvas` |

With `--yes`, or when the input is not a terminal, it takes the defaults
without asking (and fails when a default is not accepted, e.g. a directory
name with a space: give `--name`).

It writes the template's files that are not there yet; one that is there is
kept (`(kept: it is already here)`), except `.gitignore`, which gets the
lines it lacks.  `miso.yml` gets `synopsis`, `author` and `license` (no
`license:` for `none`), and a `LICENSE` file is written for `MIT`,
`BSD-2-Clause` and `BSD-3-Clause`.

Fails in a directory that has `miso.yml` already, and on an unknown flag.

### miso build

```
miso build [NAME...]
```

Builds the project: type checks the library, if there is one (`mhs
-fno-code`), and builds every program; or, with names, only those programs
(and the library is not checked).

- A `native` program (a program without `target:` is one) is built to
  `dist/NAME`.
- A browser program (`browser`: WebAssembly, `browser_js`: JavaScript) is
  built to `public/index.js`, next to a copy of its `static-dir` (default
  `static/`); with several browser programs, each to `public/NAME/`.
- Tests are not built ([`miso test`](#miso-test) builds them).

Before installing anything it checks that `emcc` is there when a program
needs it (targets `browser`, `browser_js`, `browser_rts`, `emscripten`,
`emscripten_js`, `emscripten_web`), and says where to get it.  On a
terminal it shows the build as it runs, then what mhs said (warnings, or
the error); the whole log is `.miso-build.log`.

Fails when a NAME is not a program of `miso.yml`, a `static-dir` other than
`static` does not exist, the library does not type check, or a build fails.

`miso build --dev NAME` is the build [`miso dev`](#miso-dev) runs: for a
`browser` program with a MicroHs that has the browser runtime, it writes the
program as combinators (`app.comb`) and a runtime built once with emcc
(kept in `$MISO_HOME/rts`; no emcc is needed when it is there already).

### miso run

```
miso run [NAME] [ARGS...]
miso run [NAME] -- ARGS...
```

Builds a native program and runs it, with `ARGS`: the program named, when
the first argument is the name of one, otherwise the first `native` program
of `miso.yml`.  A `--` before the arguments is dropped.  It exits with the
program's exit status.

Fails for a browser program (use [`miso dev`](#miso-dev) or
[`miso serve`](#miso-serve)), and in a project with no programs.

### miso test

```
miso test [NAME...] [-- ARGS...]
```

Builds the tests of `miso.yml` (the `tests:` section; or those named) to
`dist/NAME`, and runs each with `ARGS`.  A test passes when it exits with 0.
All of them run; then it says how many passed, and fails when one did not
(`1 of 2 tests failed: fail`).

Fails when `miso.yml` has no `tests:`, or a NAME is not a test.

### miso dev

```
miso dev [NAME] [--port N]
miso dev --port N NAME
```

For a browser program (the one named, or the first browser program):
serves `public/` on `http://localhost:PORT/` (127.0.0.1), builds the
program, and builds it again whenever one of these changes, reloading the
page when the build works:

- the source dirs of the program and of the library,
- its `static-dir`,
- `cbits/`, `jsbits/`, `include/`, and the dirs of its C and JavaScript
  files,
- `miso.yml`, and the directories of `path:` dependencies.

PORT is N, or the first free port from 8000 to 8099.  A build that fails
shows its log and keeps the last build in the page.  It runs until Ctrl-C.

With a MicroHs that has the browser runtime (target `browser_rts`), a save
does not run emcc: see `miso build --dev` above, and the README.

Fails for a native program, when no port is free (or N is taken), and when
`emcc` is not installed (for a `browser` program, unless a runtime was
built before in `$MISO_HOME/rts`).

### miso serve

```
miso serve [--port N]
```

Serves `public/` as it is (no build, no reload) on N, or the first free
port from 8000.  It runs until Ctrl-C.  Fails when there is no `public/`.

### miso watch

```
miso watch [NAME]
```

Type checks the library, the programs and the tests (or the program or test
named) with `mhs -fno-code`, then again on every change of their sources.
On a terminal it clears the screen each time.  It shows `All good: ...` or
the first error, as `file:line:col: message`; mhs stops at the first error,
so the next one shows after that one is fixed.  It installs the
dependencies first, and runs until Ctrl-C.

### miso lint

```
miso lint [ARGS...]
```

Runs [hlint](https://github.com/ndmitchell/hlint) on the source dirs of
`miso.yml` that exist, with `ARGS` (hlint reads `.hlint.yaml`), and exits
with hlint's status (1 when it has hints).  Fails when hlint is not
installed.

### miso repl

```
miso repl
```

MicroHs interactively (`mhs --interactive`).  In a project, with its source
dirs and `mhs-options`, after installing its dependencies: `import` a
module of the project to use it.  Outside a project, with the packages that
come with the tool.  When the input is not a terminal it reads plain lines,
so expressions can be piped to it:

```sh
printf 'import Calc\nanswer + 1\n' | miso repl
```

`:quit` leaves; `:help` lists the other commands.

### miso docs

```
miso docs [--out DIR] [--no-types]
```

A documentation site for the library of `miso.yml`, as Haddock makes, in
`docs/` (or `DIR`): its `exposed-modules`, or every module of its
`source-dirs`.  It reads the modules' source (after CPP, with the library's
`cpp-options`), and type checks the library, after installing its
dependencies, for the types the source does not say; `--no-types` skips
both.  When the library does not type check, it warns and goes on with
the source alone.  The site works from `file://` or any server:
`index.html`, the viewer (`index.js`, a miso app), `data/MODULE.js` for each
module, and `NAME.txt`, a Hoogle database.  See the
[README](../README.md) for what it shows.

The viewer does not depend on the library: it is built with emcc the first
time, and kept in `$MISO_HOME/docs-viewer/`.  Fails when `miso.yml` has no
library, the library has no modules, a module has no source in the source
dirs, or the viewer is not built yet and emcc is not installed.

### miso clean

```
miso clean
```

Removes what builds make in the project: `dist/`, `public/`, `.miso/`
(installed packages), `.mhscache`, `dist-mcabal/` and `.miso-build.log`.
`.miso/` is kept when it is `$MISO_HOME` or where the tool is installed (a
project in your home directory).  Note that `public/` is removed entirely.

Fails outside a project, so that it does not remove another kind of
project's `public/` or `dist/`.

---

## Dependencies

The dependencies are the `dependencies:` of `miso.yml` (Hackage packages,
`git:`, `path:`); see the README's [miso.yml](../README.md#misoyml) and
[Packages](../README.md#packages).

### miso install

```
miso install
```

Installs the dependencies of `miso.yml` in the project's `.miso/`, each
after the packages it needs, unless they are installed already
(`dependencies are installed`):

- with `miso.lock`, exactly what it locks (Hackage tarballs checked against
  their sha256, git commits); a dependency it does not lock yet is locked
  for this install, with a warning;
- without `miso.lock`, locked for this install as `miso lock` would, but
  without writing `miso.lock`;
- in a vendored project, from `vendor/` (see [vendor](#miso-vendor)).

A package built before, with the same source, flags and compiler, against
the same packages, is copied from the store (`$MISO_HOME/store`) instead of
built.  `path:` dependencies are installed again when their files change.

Fails when a package cannot be downloaded or built, when a download does not
match `miso.lock`'s sha256, or when `miso.yml` replaces a package of the
compiler (`base`, ...).

### miso add

```
miso add PKG...
```

Adds Hackage packages to `dependencies:` of `miso.yml` (keeping its
comments and layout), and installs them.  A package that is a dependency
already is skipped.  When the install fails, `miso.yml` is written back as
it was (`Not added: PKG (miso.yml is as it was)`).  For `git:`, `path:` or a
`version:`, edit `miso.yml`.

### miso remove

```
miso remove PKG...
```

Removes dependencies from `miso.yml`.  The packages stay installed in
`.miso/` until [`miso clean`](#miso-clean).  A PKG that is not a dependency
gets a warning.

### miso list

```
miso list
```

Lists the packages that come with the tool (marked `part of the compiler`,
or `replaced in this project`), and in a project, the packages installed in
its `.miso/` (marked `instead of the one that comes with miso` when they
replace one).

### miso lock

```
miso lock
```

Writes `miso.lock`: the dependencies of `miso.yml` and every package they
need, in install order, with the version and sha256 of each Hackage package
and the commit of each git one.  It locks what is not locked yet, keeps what
is (at its version), and drops what is no longer needed; it lists the
changes, or says `miso.lock is up to date`.  See the README's
[miso.lock](../README.md#misolock).

### miso update

```
miso update
```

Gets a new package list (the newest Stackage snapshot), locks every package
again at its version and every git dependency at the current commit of its
`rev`, writes `miso.lock`, and installs.

### miso vendor

```
miso vendor
```

Runs [`miso lock`](#miso-lock), then copies the source of every locked
package to `vendor/NAME` and lists them, with a sha256 of their files, in
`vendor/.miso-vendor`.  A package that is there unchanged is kept, one that
changed is copied again, and one no longer needed is removed.  Builds then
install from `vendor/` and never download.  See the README's
[vendor/](../README.md#vendor).

### miso cabal

```
miso cabal
```

Prints the `.cabal` file of the project's `library:`: what another project
that depends on this one (with `git:` or `path:`) builds it with.

Fails without a `library:` section, when the library has no modules, or
when the project needs a package from `git:` or a `path:` (a project that
uses it cannot fetch those for it yet).

---

## The tool

### miso doctor

```
miso doctor
```

Checks the installation (the package database, `base`, miso, `cpphs`, that
`$MISO_HOME` is writable), the tools (`cc`, `curl`, `tar`, `sha256sum` or
`shasum`; and, as notes, `hlint`, `git` and `emcc`, and the JS runtime
`emcc` runs on every build: the one `em-config NODE_JS` names, from
`EM_NODE_JS` or the emscripten config, smoke-tested so `bun` counts too),
and in a project, `miso.yml`, its programs and tests, source dirs, `emcc`
and its JS runtime for its browser targets, `.miso/`, `path:`
dependencies, and `git` for git dependencies.
It says how to fix each problem, and fails when it found one (`2 problems
found`); a missing optional tool is a note, not a problem.

### miso upgrade

```
miso upgrade [VERSION]
```

Installs the latest release (or `VERSION`, e.g. `v0.1.0`) where this one is
installed, by downloading `install.sh` and running it with `MISO_HOME` set
to the installation and `MISO_VERSION` to VERSION.  The installer is
downloaded from `MISO_INSTALL_URL`, by default
`https://raw.githubusercontent.com/haskell-miso/cli/main/install.sh`.  The
packages projects installed are installed again by their next build.

Fails when the installer cannot be downloaded, or when it fails.

### miso uninstall

```
miso uninstall [--yes]
```

Removes miso: its files in `$MISO_HOME` (packages, caches, the store) and
the tool where `install.sh` put it (`bin/miso`, `bin/cpphs`, the package
database).  It lists what it removes, with sizes, and asks first; `--yes`
does not ask, and is needed when the input is not a terminal.  A folder
that holds other files keeps them; your home directory, `/`, and a tool
installed with nix are never removed.  It lists the lines of your shell
startup files that add miso to `PATH`, for you to remove.

### miso version

```
miso version
miso --version
```

Prints `miso VERSION (MicroHs VERSION)`.

### miso help

```
miso help
miso --help
miso
```

Prints the commands.  An unknown command prints them too, and fails.

`miso COMMAND --help` (or `-h`) prints the usage of that command, and does
not run it.  For `run` and `test`, which give their arguments to the
program, only `--help` right after the command counts: `miso run app
--help` runs `app --help`.

### miso mhs

```
miso mhs ARGS...
```

Runs the MicroHs compiler that is built into the tool, with the project's
`.miso/` packages and the tool's as its package path, and nothing else.
When ARGS has a bare `-a` (which clears mhs's package path), miso leaves
the package path to ARGS.

### miso mcabal

```
miso mcabal ARGS...
```

Runs MicroCabal, which compiles with `miso mhs`.

---

## Environment variables

| variable | |
|---|---|
| `MISO_HOME` | where miso keeps downloads, git checkouts, the package list, the store and dev runtimes (default `~/.miso`); `install.sh` installs there too |
| `NO_COLOR` | set (non-empty) to turn off colors |
| `MISO_INSTALL_URL` | where `miso upgrade` downloads `install.sh` from |
| `MISO_VERSION` | for `install.sh`: the release to install (default the latest) |
| `MISO_RELEASE_URL` | for `install.sh`: a mirror of the release files (or `file://`) |
| `MISO_REPO` | for `install.sh`: the GitHub repository of the releases (default `haskell-miso/cli`) |
| `MHSCPPHS` | the `cpphs` mhs runs for modules with CPP (default the one next to `miso`) |

miso sets `MHS`, `CABALDIR` and `MISO_PROJECT` for the tools it runs.

## Files and directories

In a project:

| | |
|---|---|
| `miso.yml` | the project (see the README) |
| `miso.lock` | the locked dependencies; commit it |
| `vendor/` | vendored sources (`miso vendor`); commit it |
| `.miso/` | installed packages |
| `dist/` | native programs and tests |
| `public/` | browser programs, and their static files |
| `.miso-build.log` | the log of the last build |

In `$MISO_HOME` (default `~/.miso`):

| | |
|---|---|
| `bin/miso`, `bin/cpphs`, `mhs-VERSION/` | the tool and its package database (when installed with `install.sh`) |
| `src/` | downloads and git checkouts |
| `mcabal/` | MicroCabal's package list |
| `store/` | built packages, shared by projects |
| `rts/` | the browser runtimes of `miso dev` |
