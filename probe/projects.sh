#!/bin/sh
# Project shapes: a library with tests, add/remove/lock/update/list, a git
# dependency, a local path dependency, and a browser program with a package.
#   probe/projects.sh TARBALL
set -u
tarball=$1
tmp=$(mktemp -d)
tar -C "$tmp" -xzf "$tarball"
export MISO_HOME=$tmp/home PATH="$(ls -d "$tmp"/miso-*)/bin:$PATH"
cd "$tmp"
ok() { echo "PROBE ok: $1"; }
bad() { echo "PROBE FAIL: $1"; [ -f "${2:-}" ] && grep -v '^\s*$' "$2" | grep -v 'callCommand: failed\|^"' | tail -4 | cut -c1-200; }
try() { name=$1; shift; if "$@" > "$tmp/$name.log" 2>&1; then ok "$name"; else bad "$name" "$tmp/$name.log"; fi; }

# 1. a library, a program and a test, each with dependencies of its own
miso new lib > /dev/null 2>&1; cd lib
cat > miso.yml <<'YML'
name: lib
version: 0.1.0
# top-level dependencies: for every component
dependencies: [split]
library:
  source-dirs: [src]
  dependencies: [data-default]
executables:
  app:
    main: Main
    source-dirs: [app]
    target: native
    dependencies: [time]
tests:
  spec:
    main: Spec
    source-dirs: [test]
    dependencies: [random]
YML
mkdir -p src test
printf 'module Lib (firstField) where\nimport Data.List.Split (splitOn)\nimport Data.Default (def)\nfirstField :: String -> String\nfirstField s = case splitOn "," s of { x : _ -> x; [] -> def }\n' > src/Lib.hs
printf 'module Main where\nimport Lib (firstField)\nimport Data.Time.Calendar (fromGregorian)\nmain :: IO ()\nmain = fromGregorian 2026 1 1 `seq` putStrLn (firstField "lib,b")\n' > app/Main.hs
printf 'module Main where\nimport Lib (firstField)\nimport System.Random (mkStdGen)\nimport System.Exit\nmain :: IO ()\nmain = mkStdGen 1 `seq` if firstField "a,b" == "a" then putStrLn "spec ok" else exitFailure\n' > test/Spec.hs
try "library: miso build" miso build
try "library: miso run app" sh -c 'miso run app | tail -1 | grep -qx lib'
try "library: miso test" miso test
try "library: miso lock" sh -c 'miso lock && for p in split data-default time random; do grep -q "name: $p\$" miso.lock || { echo "miso.lock lacks $p"; cat miso.lock; exit 1; }; done'
try "miso add hashable" sh -c 'miso add hashable && grep -q hashable miso.yml && grep -q "# top-level dependencies" miso.yml'
try "miso remove hashable" sh -c 'miso remove hashable && ! grep -q hashable miso.yml && grep -q "dependencies: \[split\]" miso.yml'
try "miso list" sh -c 'miso list | grep -q split'
try "miso update" miso update
try "miso vendor" sh -c 'miso vendor && ls vendor | grep -q split'
try "build after vendor" miso build
cd "$tmp"

# 2. a git dependency at a tag
miso new gitdep > /dev/null 2>&1; cd gitdep
cat > miso.yml <<'YML'
name: gitdep
version: 0.1.0
main: Main
source-dirs: [app]
target: native
dependencies:
  - name: split
    git: https://github.com/byorgey/split
    rev: v0.2.5
YML
printf 'module Main where\nimport Data.List.Split (chunksOf)\nmain :: IO ()\nmain = print (chunksOf 2 [1,2,3 :: Int])\n' > app/Main.hs
try "git dependency: miso run" sh -c 'miso run | tail -1 | grep -qx "\[\[1,2\],\[3\]\]"'
try "git dependency: locked at its commit" sh -c 'miso lock && grep -q "git: https://github.com/byorgey/split" miso.lock && grep -Eq "commit: [0-9a-f]{40}" miso.lock'
cd "$tmp"

# 3. a local path dependency, and a change to it
miso new mylib > /dev/null 2>&1; cd mylib
printf 'name: mylib\nversion: 0.1.0\ndependencies: [split]\nlibrary:\n  source-dirs: [src]\n' > miso.yml
mkdir -p src && printf 'module MyLib (greet) where\ngreet :: String\ngreet = "one"\n' > src/MyLib.hs
cd "$tmp"; miso new usepath > /dev/null 2>&1; cd usepath
printf 'name: usepath\nversion: 0.1.0\nmain: Main\nsource-dirs: [app]\ntarget: native\ndependencies:\n  - name: mylib\n    path: ../mylib\n' > miso.yml
printf 'module Main where\nimport MyLib (greet)\nmain :: IO ()\nmain = putStrLn greet\n' > app/Main.hs
try "path dependency: miso run" sh -c 'miso run | tail -1 | grep -qx one'
sed -i.bak 's/"one"/"two"/' ../mylib/src/MyLib.hs
try "path dependency: a change is picked up" sh -c 'miso run | tail -1 | grep -qx two'
cd "$tmp"

# 4. a browser program with a package from Hackage
if command -v emcc > /dev/null; then
  miso new web > /dev/null 2>&1; cd web
  try "browser: miso add split" miso add split
  sed -i.bak 's/^import Miso$/import Miso\nimport Data.List.Split (splitOn)/' app/Main.hs
  try "browser: miso build with split" sh -c 'miso build && test -s public/index.js'
  try "browser: miso build --dev with split" sh -c 'miso build --dev web && test -s public/app.comb'
else
  bad "browser: no emcc"
fi
