#!/bin/sh
# Unpack a release tarball, make a project with it, and run it natively.
set -eu
tarball=$1
tmp=$(mktemp -d)
tar -C "$tmp" -xzf "$tarball"
export MISO_HOME=$tmp/home PATH="$(ls -d "$tmp"/miso-*)/bin:$PATH"
cd "$tmp"
miso version
miso new hello
cd hello
# data-default-class needs data-default, which does not come with miso:
# miso installs it too (by name, then locked, then from vendor/)
sed -e 's/^target: browser/target: native/' -e 's/^dependencies: \[\]/dependencies: [split, data-default-class]/' miso.yml > miso.yml.new
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
miso serve --port 8799 > serve.log 2>&1 &
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
sed -e 's/^target: browser/target: native/' -e 's/^dependencies: \[\]/dependencies: [split]/' miso.yml > miso.yml.new
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
    version: 0.6.1.1
YML
printf 'import Control.Monad.State\nmain :: IO ()\nmain = print (execState (modify (+1)) (41 :: Int))\n' > app/Main.hs
miso run > replace.log 2>&1 || { echo "smoke test: miso run failed replacing transformers" >&2; cat replace.log >&2; exit 1; }
grep -q 'Installing transformers 0.6.1.1' replace.log || { echo "smoke test: transformers 0.6.1.1 was not installed" >&2; cat replace.log >&2; exit 1; }
grep -q 'Building mtl .* again from its source, against transformers' replace.log || { echo "smoke test: mtl was not built again" >&2; cat replace.log >&2; exit 1; }
[ "$(tail -1 replace.log | tr -d '\r')" = 42 ] || { echo "smoke test: expected 42 replacing transformers" >&2; cat replace.log >&2; exit 1; }
miso list | grep -q '^  transformers  *0.6.1.1 (instead of the one that comes with miso)' || { echo "smoke test: miso list does not show the replaced transformers" >&2; miso list >&2; exit 1; }
# replacing a package of the compiler is an error
sed -e 's/name: transformers/name: base/' -e 's/version: 0.6.1.1/version: 1.0/' miso.yml > miso.yml.new
mv miso.yml.new miso.yml
if miso install > base.log 2>&1; then echo "smoke test: replacing base did not fail" >&2; exit 1; fi
grep -q 'base comes with miso as part of its compiler' base.log || { echo "smoke test: no error for replacing base" >&2; cat base.log >&2; exit 1; }
echo "smoke test: ok"
