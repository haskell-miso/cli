#!/bin/sh
# Unpack a release tarball, make a project with it, and run it natively.
set -eu
tarball=$1
tmp=$(mktemp -d)
tar -C "$tmp" -xzf "$tarball"
export MISO_HOME=$tmp/home PATH="$(ls -d "$tmp"/miso-*)/bin:$PATH"
tool=$(ls -d "$tmp"/miso-*)
cd "$tmp"
# every command miso help lists is run below: the commands that ran are
# kept, and checked against miso help at the end
ran=$tmp/commands-ran
: > "$ran"
record() { printf '%s\n' "$1" >> "$ran"; }
miso() { record "${1:-}"; command miso "$@"; }
# (a command run in the background is recorded, and run with command miso:
# dash, as /bin/sh, runs a function in the background in a subshell, and
# kill $! stops that, not miso)
fail() { echo "smoke test: $1" >&2; [ -f "${2:-}" ] && cat "$2" >&2; exit 1; }
miso version
miso new hello
cd hello
# data-default-class needs data-default, which does not come with miso:
# miso installs it too (by name, then locked, then from vendor/)
sed -e 's/^target: browser/target: native/' -e 's/^dependencies: \[miso\]/dependencies: [split, data-default-class]/' miso.yml > miso.yml.new
mv miso.yml.new miso.yml
cat > app/Main.hs <<'HS'
{-# LANGUAGE CPP #-}
module Main where
import Data.List.Split (splitOn)
import Data.Default.Class (def)
main :: IO ()
#if 1
main = putStrLn (concat (splitOn "," "hel,lo" ++ (def :: [String])))
#else
main = putStrLn "cpp did not run"
#endif
HS
out=$(miso run | tail -1 | tr -d '\r')
[ "$out" = hello ] || { echo "smoke test: expected hello, got: $out" >&2; exit 1; }
# miso update locks split in miso.lock, and the build installs that version
miso update
grep -q '^    hackage: ' miso.lock || { echo "smoke test: miso update did not lock split" >&2; cat miso.lock >&2; exit 1; }
# and data-default, which data-default-class needs, before it (the order they install in)
dd=$(grep -n '^  - name: data-default$' miso.lock | cut -d: -f1)
ddc=$(grep -n '^  - name: data-default-class$' miso.lock | cut -d: -f1)
[ -n "$dd" ] && [ -n "$ddc" ] && [ "$dd" -lt "$ddc" ] || { echo "smoke test: miso update did not lock data-default before data-default-class" >&2; cat miso.lock >&2; exit 1; }
out=$(miso run | tail -1 | tr -d '\r')
[ "$out" = hello ] || { echo "smoke test: expected hello with miso.lock, got: $out" >&2; exit 1; }
# miso vendor copies split to vendor/, and the build installs it from there
# (without the Hackage tarball, and installed again: not in the project's
# .miso/, nor in the store)
forget_installed() {
  rm -rf .miso "$MISO_HOME/store"
}
miso vendor
grep -q '^hackage split .* files:' vendor/.miso-vendor || { echo "smoke test: miso vendor did not vendor split" >&2; ls -la vendor >&2; exit 1; }
miso vendor | grep -q 'vendor/split (kept)' || { echo "smoke test: miso vendor copied split again" >&2; exit 1; }
rm -rf "$MISO_HOME/src/hackage"
# not installed at all: data-default must be installed before data-default-class
forget_installed
miso run > vendor.log 2>&1 || { echo "smoke test: miso run failed with vendor/" >&2; cat vendor.log >&2; exit 1; }
grep -q 'Installing split from vendor/split' vendor.log || { echo "smoke test: split was not installed from vendor/" >&2; cat vendor.log >&2; exit 1; }
dd=$(grep -n 'Installing data-default from vendor/data-default ' vendor.log | cut -d: -f1)
ddc=$(grep -n 'Installing data-default-class from vendor/data-default-class ' vendor.log | cut -d: -f1)
[ -n "$dd" ] && [ -n "$ddc" ] && [ "$dd" -lt "$ddc" ] || { echo "smoke test: data-default was not installed from vendor/ before data-default-class" >&2; cat vendor.log >&2; exit 1; }
! ls "$MISO_HOME"/src/hackage/split-*.tar.gz >/dev/null 2>&1 || { echo "smoke test: split was downloaded despite vendor/" >&2; exit 1; }
out=$(tail -1 vendor.log | tr -d '\r')
[ "$out" = hello ] || { echo "smoke test: expected hello with vendor/, got: $out" >&2; exit 1; }
# an edited vendored package is an error (not a download), until miso vendor
# (a module of its own, not one mcabal generated in dist-mcabal/)
f=$(find vendor/split/src -name '*.hs' | head -1)
echo '-- edited' >> "$f"
forget_installed
if miso install > vendor.log 2>&1; then echo "smoke test: miso install used an edited vendor/split" >&2; cat vendor.log >&2; exit 1; fi
grep -q 'vendor/split has changed' vendor.log || { echo "smoke test: miso install did not report the edited vendor/split" >&2; cat vendor.log >&2; exit 1; }
miso vendor | grep -q '+ vendor/split' || { echo "smoke test: miso vendor did not copy the edited split again" >&2; exit 1; }
miso install
# miso test: a test that passes and one that fails
cat >> miso.yml <<'YML'
tests:
  pass:
    main: Pass
  fail:
    main: Fail
YML
mkdir -p test
cat > test/Pass.hs <<'HS'
module Pass(main) where
main :: IO ()
main = putStrLn "pass"
HS
cat > test/Fail.hs <<'HS'
module Fail(main) where
import System.Exit
main :: IO ()
main = exitWith (ExitFailure 3)
HS
miso test pass
if miso test > test.log 2>&1; then echo "smoke test: miso test passed with a failing test" >&2; cat test.log >&2; exit 1; fi
grep -q "1 of 2 tests failed: fail" test.log || { echo "smoke test: miso test did not report the failing test" >&2; cat test.log >&2; exit 1; }
miso doctor
# the packages are the project's, in .miso/; mhs looks there and in the tool's
# packages, nothing else
ls .miso/mcabal/mhs-*/packages/split-*.pkg >/dev/null || { echo "smoke test: split is not in .miso/" >&2; exit 1; }
miso mhs -v -v app/Main.hs -o/dev/null > path.log 2>&1 || true
# (the project's directory as mhs has it: /tmp may be a link)
grep -q 'pkgPaths = \["[^"]*/hello/\.miso/mcabal/mhs-[0-9.]*","[^"]*/mhs-[0-9.]*"\]' path.log \
  || { echo "smoke test: mhs's package path is not the project's and the tool's" >&2; grep pkgPaths path.log >&2; exit 1; }
# miso list shows what miso install installed (split), not only what comes with miso
miso list > list.log
grep -q '^  split ' list.log || { echo "smoke test: miso list does not show split" >&2; cat list.log >&2; exit 1; }
# serve: a page from public/
mkdir -p public && echo '<html><body>served</body></html>' > public/index.html
record serve
command miso serve --port 8799 > serve.log 2>&1 &
pid=$!
for i in 1 2 3 4 5 6 7 8 9 10; do
  page=$(curl -s http://127.0.0.1:8799/ || true)
  [ -n "$page" ] && break
  sleep 1
done
kill $pid 2>/dev/null || true
case "$page" in *served*) ;; *) echo "smoke test: miso serve did not serve the page: $page" >&2; cat serve.log >&2; exit 1 ;; esac
# another project with split gets it from the store, not built again
cd "$tmp"
miso new hello2 > /dev/null
cd hello2
sed -e 's/^target: browser/target: native/' -e 's/^dependencies: \[miso\]/dependencies: [split]/' miso.yml > miso.yml.new
mv miso.yml.new miso.yml
printf 'import Data.List.Split (splitOn)\nmain :: IO ()\nmain = putStrLn (concat (splitOn "," "hel,lo"))\n' > app/Main.hs
miso run > store.log 2>&1 || { echo "smoke test: miso run failed in a second project" >&2; cat store.log >&2; exit 1; }
grep -q 'Installing split .* (built already, from the store)' store.log || { echo "smoke test: split was not installed from the store" >&2; cat store.log >&2; exit 1; }
[ "$(tail -1 store.log | tr -d '\r')" = hello ] || { echo "smoke test: expected hello from the second project" >&2; cat store.log >&2; exit 1; }
# a project that replaces a package of the tool (transformers, another
# version) has the tool's libraries that need it (mtl, miso) built again
cd "$tmp"
miso new hello3 > /dev/null
cd hello3
cat > miso.yml <<'YML'
name: hello3
version: 0.1.0
main: Main
source-dirs: [app]
target: native
dependencies:
  - name: transformers
    version: 0.6.2.0
YML
printf 'import Control.Monad.State\nmain :: IO ()\nmain = print (execState (modify (+1)) (41 :: Int))\n' > app/Main.hs
miso run > replace.log 2>&1 || { echo "smoke test: miso run failed replacing transformers" >&2; cat replace.log >&2; exit 1; }
grep -q 'Installing transformers 0.6.2.0' replace.log || { echo "smoke test: transformers 0.6.2.0 was not installed" >&2; cat replace.log >&2; exit 1; }
grep -q 'Building mtl .* again from its source, against transformers' replace.log || { echo "smoke test: mtl was not built again" >&2; cat replace.log >&2; exit 1; }
[ "$(tail -1 replace.log | tr -d '\r')" = 42 ] || { echo "smoke test: expected 42 replacing transformers" >&2; cat replace.log >&2; exit 1; }
miso list | grep -q '^  transformers  *0.6.2.0 (instead of the one that comes with miso)' || { echo "smoke test: miso list does not show the replaced transformers" >&2; miso list >&2; exit 1; }
# replacing a package of the compiler is an error
sed -e 's/name: transformers/name: base/' -e 's/version: 0.6.2.0/version: 1.0/' miso.yml > miso.yml.new
mv miso.yml.new miso.yml
if miso install > base.log 2>&1; then echo "smoke test: replacing base did not fail" >&2; exit 1; fi
grep -q 'base comes with miso as part of its compiler' base.log || { echo "smoke test: no error for replacing base" >&2; cat base.log >&2; exit 1; }
# a new project depends on miso, the miso of the release: installed as it
# came with the tool (not built), and locked at its commit
cd "$tmp"
miso new hello4 > /dev/null
cd hello4
grep -q '^dependencies: \[miso\]' miso.yml || { echo "smoke test: a new project does not depend on miso" >&2; cat miso.yml >&2; exit 1; }
miso install > miso.log 2>&1 || { echo "smoke test: miso install failed in a new project" >&2; cat miso.log >&2; exit 1; }
grep -q 'Installing miso .*(built already, ' miso.log || { echo "smoke test: miso was built, not installed as it came" >&2; cat miso.log >&2; exit 1; }
ls .miso/mcabal/mhs-*/packages/miso-*.pkg >/dev/null || { echo "smoke test: miso is not in .miso/" >&2; exit 1; }
miso lock > /dev/null
grep -q '^    git: https://github.com/dmjio/miso$' miso.lock || { echo "smoke test: miso lock did not lock miso" >&2; cat miso.lock >&2; exit 1; }
# a miso project with a library: section, as a path: dependency of another
# project (built from the .cabal file miso cabal makes of it), with a
# package of its own; and a change to it, picked up
cd "$tmp"
mkdir -p greet/src
printf 'name: greet\nversion: 0.3.0\ndependencies: [split]\nlibrary:\n  source-dirs: [src]\n' > greet/miso.yml
printf 'module Greet (greet) where\nimport Data.List.Split (splitOn)\ngreet :: String\ngreet = concat (splitOn "," "he,llo")\n' > greet/src/Greet.hs
(cd greet && miso cabal) | grep -q '^  exposed-modules: Greet$' || { echo "smoke test: miso cabal did not list the library's modules" >&2; (cd greet && miso cabal) >&2; exit 1; }
miso new usegreet > /dev/null
cd usegreet
printf 'name: usegreet\nversion: 0.1.0\nmain: Main\nsource-dirs: [app]\ntarget: native\ndependencies:\n  - name: greet\n    path: ../greet\n' > miso.yml
printf 'import Greet (greet)\nmain :: IO ()\nmain = putStrLn greet\n' > app/Main.hs
miso run > greet.log 2>&1 || { echo "smoke test: a miso project as a path dependency did not build" >&2; cat greet.log >&2; exit 1; }
[ "$(tail -1 greet.log | tr -d '\r')" = hello ] || { echo "smoke test: expected hello from the greet library" >&2; cat greet.log >&2; exit 1; }
sed -e 's/he,llo/hi,there/' ../greet/src/Greet.hs > ../greet/src/Greet.hs.new && mv ../greet/src/Greet.hs.new ../greet/src/Greet.hs
miso run > greet2.log 2>&1 || { echo "smoke test: the changed greet library did not build" >&2; cat greet2.log >&2; exit 1; }
[ "$(tail -1 greet2.log | tr -d '\r')" = hithere ] || { echo "smoke test: the change to the greet library was not picked up" >&2; cat greet2.log >&2; exit 1; }
[ ! -e ../greet/greet.cabal ] || { echo "smoke test: miso wrote a .cabal file into the dependency's folder" >&2; exit 1; }
# extra-libraries: a C library linked into a native program (zlib, where
# the C compiler has it); mhs puts -l before the program's C file, which
# GNU ld ignores unless told otherwise
if printf 'int main(void) { return 0; }\n' | cc -x c - -lz -o "$tmp/ccz" 2>/dev/null; then
  cd "$tmp"
  miso new zlib > /dev/null
  cd zlib
  printf 'name: zlib\nversion: 0.1.0\nmain: Main\nsource-dirs: [app]\ntarget: native\nextra-libraries: [z]\ndependencies: []\n' > miso.yml
  cat > app/Main.hs <<'HS'
module Main where
import Foreign.C.String (CString, peekCString)
foreign import ccall "zlib.h zlibVersion" zlibVersion :: IO CString
main :: IO ()
main = zlibVersion >>= peekCString >>= putStrLn . ("zlib " ++)
HS
  miso run > zlib.log 2>&1 || { echo "smoke test: a program with extra-libraries: [z] did not build" >&2; cat zlib.log >&2; exit 1; }
  grep -q '^zlib [0-9]' zlib.log || { echo "smoke test: the zlib program printed no version" >&2; cat zlib.log >&2; exit 1; }
else
  echo "smoke test: no zlib for the C compiler here, extra-libraries not checked"
fi
# versions: a package from Hackage gets a version that the packages that
# need it take, and one that a package does not take (of miso.yml, one
# that comes with miso, or miso) is an error, before anything is built.
# (miso lock only: these packages are not built.)
cd "$tmp"
mkdir oldsplit
cat > oldsplit/oldsplit.cabal <<'CABAL'
cabal-version: 2.4
name: oldsplit
version: 0.1.0
library
  exposed-modules: OldSplit
  build-depends: base, split <0.2
CABAL
miso new vers > /dev/null
cd vers
printf 'name: vers\nversion: 0.1.0\nmain: Main\nsource-dirs: [app]\ntarget: native\ndependencies:\n  - name: oldsplit\n    path: ../oldsplit\n' > miso.yml
miso lock > lock.log 2>&1 || fail "miso lock failed with a package that takes split <0.2" lock.log
grep -A1 '^  - name: split$' miso.lock | grep -q 'hackage: 0\.1\.' || fail "split was not locked at a version that oldsplit takes" miso.lock
grep -q 'split 0\.1\.[0-9.]*, not .* of mcabal.s package list: oldsplit-0\.1\.0 takes <0\.2' lock.log || fail "miso lock did not say why split is not the package list's" lock.log
# a package read first takes the package list's split (>=0.1), and one read
# after it does not (<0.2): miso lock resolves again, with both ranges
cd "$tmp"
mkdir anysplit
cat > anysplit/anysplit.cabal <<'CABAL'
cabal-version: 2.4
name: anysplit
version: 0.1.0
library
  exposed-modules: AnySplit
  build-depends: base, split >=0.1
CABAL
miso new vers2 > /dev/null
cd vers2
printf 'name: vers2\nversion: 0.1.0\nmain: Main\nsource-dirs: [app]\ntarget: native\ndependencies:\n  - name: anysplit\n    path: ../anysplit\n  - name: oldsplit\n    path: ../oldsplit\n' > miso.yml
miso lock > lock.log 2>&1 || fail "miso lock did not resolve again for split >=0.1 and split <0.2" lock.log
grep -q 'resolving again' lock.log || fail "miso lock did not say it resolved again" lock.log
grep -A1 '^  - name: split$' miso.lock | grep -q 'hackage: 0\.1\.' || fail "split was not locked at a version both packages take" miso.lock
cd "$tmp/vers"
cp miso.lock miso.lock.before
printf 'name: vers\nversion: 0.1.0\nmain: Main\nsource-dirs: [app]\ntarget: native\ndependencies:\n  - name: split\n    version: 0.2.5.1\n  - name: oldsplit\n    path: ../oldsplit\n' > miso.yml
if miso lock > lock.log 2>&1; then fail "miso lock locked split 0.2.5.1 for a package that takes split <0.2" lock.log; fi
grep -q 'oldsplit-0\.1\.0 needs split <0\.2, but split is 0\.2\.5\.1 (version: of miso\.yml)' lock.log || fail "no error for a version of miso.yml that a package does not take" lock.log
[ "$(cat miso.lock)" = "$(cat miso.lock.before)" ] || fail "a miso lock that failed changed miso.lock" miso.lock
cd "$tmp"
mkdir oldtool
cat > oldtool/oldtool.cabal <<'CABAL'
cabal-version: 2.4
name: oldtool
version: 0.1.0
library
  exposed-modules: OldTool
  build-depends: base, containers <0.7, miso <0.1
CABAL
cd vers
printf 'name: vers\nversion: 0.1.0\nmain: Main\nsource-dirs: [app]\ntarget: native\ndependencies:\n  - name: oldtool\n    path: ../oldtool\n' > miso.yml
if miso lock > lock.log 2>&1; then fail "miso lock locked packages that oldtool does not take" lock.log; fi
grep -q 'oldtool-0\.1\.0 needs containers <0\.7, but containers is .* (it comes with miso)' lock.log || fail "no error for a version of the tool's containers that a package does not take" lock.log
grep -q 'oldtool-0\.1\.0 needs miso <0\.1, but miso is .* (git: https://github.com/dmjio/miso at ' lock.log || fail "no error for a version of miso that a package does not take" lock.log
# help, --help and no arguments show the usage; an unknown command fails
cd "$tmp"
for a in help --help; do
  miso "$a" > help.log 2>&1 || fail "miso $a failed" help.log
  grep -q 'Usage:' help.log || fail "miso $a shows no usage" help.log
done
command miso > help.log 2>&1 || fail "miso with no arguments failed" help.log
grep -q 'Usage:' help.log || fail "miso with no arguments shows no usage" help.log
if miso frobnicate > help.log 2>&1; then fail "miso frobnicate did not fail" help.log; fi
grep -q 'unknown command: miso frobnicate' help.log || fail "no error for an unknown command" help.log
[ "$(miso --version)" = "$(miso version)" ] || fail "miso --version is not miso version"
# templates, and new: the other spelling of --template, an unknown
# template, a project that is there already
miso templates > templates.log
grep -q '^  counter ' templates.log || fail "miso templates does not list counter" templates.log
miso new --template counter hello5 > /dev/null
[ -f hello5/miso.yml ] || fail "miso new --template T NAME made no project"
if miso new hello5 > new.log 2>&1; then fail "miso new made a project over another" new.log; fi
grep -q 'hello5 already exists' new.log || fail "no error for a project that is there" new.log
if miso new hello6 --template nope > new.log 2>&1; then fail "miso new took an unknown template" new.log; fi
grep -q 'no template nope' new.log || fail "no error for an unknown template" new.log
[ ! -e hello6 ] || fail "miso new made a project of an unknown template"
# init: in a folder with files of its own (a .gitignore gets the lines it
# lacks); with the flags, and without a terminal, the defaults
mkdir initp
cd initp
printf 'notes.txt\n' > .gitignore
miso init --yes --license MIT --author 'A. Person' > init.log 2>&1 || fail "miso init failed" init.log
grep -q '^name: initp$' miso.yml || fail "miso init did not name the project after its folder" miso.yml
grep -q '^author: A. Person$' miso.yml || fail "miso init did not take --author" miso.yml
grep -q '^license: MIT$' miso.yml || fail "miso init did not take --license" miso.yml
grep -q 'MIT License' LICENSE || fail "miso init wrote no MIT LICENSE" LICENSE
grep -q '^notes.txt$' .gitignore && grep -q '^dist' .gitignore || fail "miso init did not add to .gitignore" .gitignore
if miso init --yes > init.log 2>&1; then fail "miso init ran in a project" init.log; fi
grep -q 'already here' init.log || fail "no error for miso init in a project" init.log
cd "$tmp"
mkdir initq
(cd initq && miso init --name other < /dev/null > init.log 2>&1) || fail "miso init without a terminal failed" initq/init.log
grep -q '^name: other$' initq/miso.yml && grep -q '^license: BSD-3-Clause$' initq/miso.yml || fail "miso init did not take the defaults" initq/miso.yml
# a native project: build (all, and by name), add, remove, watch, repl,
# lint, clean
cd "$tmp"
miso new cmds > /dev/null
cd cmds
sed -e 's/^target: browser/target: native/' -e 's/^dependencies: \[miso\]/dependencies: []/' miso.yml > miso.yml.new
mv miso.yml.new miso.yml
printf 'module Main where\nmain :: IO ()\nmain = putStrLn "cmds"\n' > app/Main.hs
miso build > build.log 2>&1 || fail "miso build failed" build.log
[ -x dist/cmds ] || [ -x dist/cmds.exe ] || fail "miso build made no dist/cmds" build.log
[ "$(./dist/cmds | tr -d '\r')" = cmds ] || fail "dist/cmds does not run"
miso build cmds > build.log 2>&1 || fail "miso build cmds failed" build.log
if miso build nope > build.log 2>&1; then fail "miso build built a program that is not there" build.log; fi
if miso dev > dev.log 2>&1; then fail "miso dev served a native program" dev.log; fi
grep -q 'miso dev is for browser targets' dev.log || fail "no error for miso dev of a native program" dev.log
cp miso.yml miso.yml.before
miso add split > add.log 2>&1 || fail "miso add split failed" add.log
grep -q 'split' miso.yml || fail "miso add did not add split to miso.yml" miso.yml
grep -q 'Added split' add.log || fail "miso add did not say Added" add.log
miso list | grep -q '^  split ' || fail "miso add did not install split"
miso add split > add.log 2>&1 || fail "miso add split, again, failed" add.log
grep -q 'split is already a dependency' add.log || fail "miso add added split twice" add.log
miso remove split > remove.log 2>&1 || fail "miso remove split failed" remove.log
! grep -q 'split' miso.yml || fail "miso remove did not remove split" miso.yml
miso remove nope > remove.log 2>&1 || true
grep -q 'nope is not a dependency' remove.log || fail "no warning removing what is not a dependency" remove.log
# an add that cannot install leaves miso.yml as it was
if miso add no-such-package-at-all > add.log 2>&1; then fail "miso add installed a package that does not exist" add.log; fi
[ "$(cat miso.yml)" = "$(cat miso.yml.before)" ] || fail "a failed miso add changed miso.yml" miso.yml
grep -q 'Not added: no-such-package-at-all' add.log || fail "a failed miso add did not say Not added" add.log
# watch: All good, then a type error when a source changes, then All good
record watch
command miso watch > watch.log 2>&1 &
pid=$!
wait_for() { # PATTERN LOG: wait up to two minutes for the line
  i=0
  while ! grep -q "$1" "$2" 2>/dev/null; do
    i=$((i + 1))
    [ $i -le 240 ] || return 1
    sleep 0.5
  done
}
wait_for 'All good' watch.log || { kill $pid 2>/dev/null; fail "miso watch did not type check" watch.log; }
sleep 1
printf 'module Main where\nmain :: IO ()\nmain = putStrLn True\n' > app/Main.hs
wait_for 'type error' watch.log || { kill $pid 2>/dev/null; fail "miso watch did not see the type error" watch.log; }
sleep 1
n=$(grep -c 'All good' watch.log)
printf 'module Main where\nmain :: IO ()\nmain = putStrLn "cmds"\n' > app/Main.hs
i=0
while [ "$(grep -c 'All good' watch.log)" -le "$n" ] && [ $i -lt 240 ]; do i=$((i + 1)); sleep 0.5; done
kill $pid 2>/dev/null || true
[ "$(grep -c 'All good' watch.log)" -gt "$n" ] || fail "miso watch did not see the fix" watch.log
# repl: expressions from a pipe, with a module of the project's sources
printf 'module Calc where\nanswer :: Int\nanswer = 42\n' > app/Calc.hs
printf 'import Calc\nanswer + 1\n:quit\n' | miso repl > repl.log 2>&1 || fail "miso repl failed" repl.log
tr -d '\r' < repl.log | grep -q '> 43$' || fail "miso repl did not evaluate answer + 1 of the project" repl.log
rm app/Calc.hs
# lint: hlint where it is installed, and an error where it is not
if command -v hlint > /dev/null; then
  miso lint > lint.log 2>&1 || [ $? -eq 1 ] || fail "miso lint failed" lint.log
  grep -q 'Linting app' lint.log || fail "miso lint did not lint app" lint.log
else
  if miso lint > lint.log 2>&1; then fail "miso lint ran without hlint" lint.log; fi
  grep -q 'runs hlint, which is not installed' lint.log || fail "no error for miso lint without hlint" lint.log
fi
# clean: what builds made, in a project only
mkdir -p public && touch public/index.html
miso clean > clean.log 2>&1 || fail "miso clean failed" clean.log
for d in dist public .miso; do [ ! -e "$d" ] || fail "miso clean left $d" clean.log; done
[ -f miso.yml ] && [ -f app/Main.hs ] || fail "miso clean removed the project's files"
miso clean | grep -q 'nothing to clean' || fail "miso clean, again, did not say nothing to clean"
cd "$tmp"
if miso clean > clean.log 2>&1; then fail "miso clean ran outside a project" clean.log; fi
grep -q 'no miso.yml here' clean.log || fail "no error for miso clean outside a project" clean.log
# serve: no public/ is an error
if (cd initq && miso serve --port 8798) > serve.log 2>&1; then fail "miso serve served no public/" serve.log; fi
grep -q 'no public/ here' serve.log || fail "no error for miso serve without public/" serve.log
# a browser program without emcc: build and dev say what to install,
# and dev stops before serving (with emcc, scripts/template-test.sh runs
# miso dev)
if ! command -v emcc > /dev/null; then
  miso new web > /dev/null
  cd web
  if miso build > build.log 2>&1; then fail "miso build built a browser program without emcc" build.log; fi
  grep -q 'which needs emcc (emscripten), and it is not installed' build.log || fail "no error for miso build without emcc" build.log
  if miso dev --port 8798 > dev.log 2>&1; then fail "miso dev ran without emcc" dev.log; fi
  grep -q 'which needs emcc' dev.log || fail "no error for miso dev without emcc" dev.log
  ! grep -q 'Serving' dev.log || fail "miso dev served without emcc" dev.log
  sed 's/^target: browser$/target: emscripten_web/' miso.yml > miso.yml.new
  mv miso.yml.new miso.yml
  if miso build > build.log 2>&1; then fail "miso build built for emscripten_web without emcc" build.log; fi
  grep -q 'target: emscripten_web, which needs emcc' build.log || fail "no error for emscripten_web without emcc" build.log
  cd "$tmp"
fi
# mcabal, as the tool runs it
miso mcabal --version > mcabal.log 2>&1 || fail "miso mcabal --version failed" mcabal.log
grep -q '[0-9]' mcabal.log || fail "miso mcabal --version printed no version" mcabal.log
# without the network (a proxy that refuses): a project of the tool's
# packages installs and builds, and a download that fails says so
offline() {
  MISO_HOME=$tmp/offline-home http_proxy=http://127.0.0.1:9 https_proxy=http://127.0.0.1:9 \
    HTTP_PROXY=http://127.0.0.1:9 HTTPS_PROXY=http://127.0.0.1:9 ALL_PROXY=http://127.0.0.1:9 "$@"
}
miso new off > /dev/null
cd off
sed -e 's/^target: browser/target: native/' miso.yml > miso.yml.new
mv miso.yml.new miso.yml
printf 'module Main where\nmain :: IO ()\nmain = putStrLn "offline"\n' > app/Main.hs
offline miso run > off.log 2>&1 || fail "a project of the tool's packages did not build offline" off.log
[ "$(tail -1 off.log | tr -d '\r')" = offline ] || fail "expected offline from the offline build" off.log
cp miso.yml miso.yml.before
if offline miso add split > off.log 2>&1; then fail "miso add split worked offline" off.log; fi
grep -q 'error: could not download .* (check the network connection, or the proxy)' off.log || fail "a failed download is not said as one" off.log
! grep -q 'uncaught exception' off.log || fail "a failed download is an uncaught exception" off.log
[ "$(cat miso.yml)" = "$(cat miso.yml.before)" ] || fail "a failed miso add changed miso.yml offline" miso.yml
cd "$tmp"
# upgrade: runs the installer (MISO_INSTALL_URL) with MISO_HOME set to the
# tool's folder, and MISO_VERSION to the version asked for
cat > fake-install.sh <<'SH'
printf '%s %s\n' "$MISO_HOME" "${MISO_VERSION:-latest}" > "$UPGRADE_LOG"
SH
UPGRADE_LOG=$tmp/upgrade.out MISO_INSTALL_URL="file://$tmp/fake-install.sh" miso upgrade 9.9.9 > upgrade.log 2>&1 || fail "miso upgrade failed" upgrade.log
read -r home ver < upgrade.out
[ "$ver" = 9.9.9 ] || fail "miso upgrade did not ask for 9.9.9" upgrade.out
[ "$(cd "$home" && pwd -P)" = "$(cd "$tool" && pwd -P)" ] || fail "miso upgrade did not install where the tool is" upgrade.out
if MISO_INSTALL_URL="file://$tmp/no-such-installer.sh" miso upgrade > upgrade.log 2>&1; then fail "miso upgrade worked without an installer" upgrade.log; fi
grep -q 'could not download the installer' upgrade.log || fail "no error for an installer that is not there" upgrade.log
printf 'exit 3\n' > failing-install.sh
if MISO_INSTALL_URL="file://$tmp/failing-install.sh" miso upgrade > upgrade.log 2>&1; then fail "miso upgrade worked with a failing installer" upgrade.log; fi
grep -q 'the upgrade failed' upgrade.log || fail "no error for a failing installer" upgrade.log
# the commands of miso help, for the check below (uninstall removes miso)
commands="$(NO_COLOR=1 command miso help | awk '/^  [a-z]/ { print $1 }') templates help"
# uninstall: asks first (without a terminal, --yes), then removes
# $MISO_HOME and the tool
if miso uninstall < /dev/null > uninstall.log 2>&1; then fail "miso uninstall did not ask first" uninstall.log; fi
grep -q 'run miso uninstall --yes' uninstall.log || fail "no error for miso uninstall without a terminal" uninstall.log
[ -x "$tool/bin/miso" ] || [ -x "$tool/bin/miso.exe" ] || fail "miso uninstall without --yes removed the tool"
miso uninstall --yes > uninstall.log 2>&1 || fail "miso uninstall --yes failed" uninstall.log
grep -q 'Removed miso' uninstall.log || fail "miso uninstall did not say Removed" uninstall.log
[ ! -e "$MISO_HOME" ] || fail "miso uninstall left \$MISO_HOME" uninstall.log
[ ! -e "$tool/bin/miso" ] && [ ! -e "$tool/bin/miso.exe" ] || fail "miso uninstall left the tool" uninstall.log
# every command of miso help ran
missing=
for c in $commands; do
  grep -qx -- "$c" "$ran" || missing="$missing $c"
done
[ -z "$missing" ] || fail "commands of miso help that this test does not run:$missing"
echo "smoke test: ok"
