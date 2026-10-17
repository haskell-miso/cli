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
sed -e 's/^target: browser/target: native/' -e 's/^dependencies: \[\]/dependencies: [split]/' miso.yml > miso.yml.new
mv miso.yml.new miso.yml
cat > app/Main.hs <<'HS'
{-# LANGUAGE CPP #-}
module Main where
import Data.List.Split (splitOn)
main :: IO ()
#if 1
main = putStrLn (concat (splitOn "," "hel,lo"))
#else
main = putStrLn "cpp did not run"
#endif
HS
out=$(miso run | tail -1 | tr -d '\r')
[ "$out" = hello ] || { echo "smoke test: expected hello, got: $out" >&2; exit 1; }
# miso update locks split in miso.lock, and the build installs that version
miso update
grep -q '^    hackage: ' miso.lock || { echo "smoke test: miso update did not lock split" >&2; cat miso.lock >&2; exit 1; }
out=$(miso run | tail -1 | tr -d '\r')
[ "$out" = hello ] || { echo "smoke test: expected hello with miso.lock, got: $out" >&2; exit 1; }
# miso vendor copies split to vendor/, and the build installs it from there
# (without the Hackage tarball, and installed again: no record of it in
# miso-installed, next to the tool or in $MISO_HOME/mcabal)
forget_installed() {
  rm -f "$(ls -d "$tmp"/miso-*)"/mhs-*/miso-installed "$MISO_HOME"/mcabal/mhs-*/miso-installed*
}
miso vendor
grep -q '^hackage split .* files:' vendor/.miso-vendor || { echo "smoke test: miso vendor did not vendor split" >&2; ls -la vendor >&2; exit 1; }
miso vendor | grep -q 'vendor/split (kept)' || { echo "smoke test: miso vendor copied split again" >&2; exit 1; }
rm -rf "$MISO_HOME/src/hackage"
forget_installed
miso run > vendor.log 2>&1 || { echo "smoke test: miso run failed with vendor/" >&2; cat vendor.log >&2; exit 1; }
grep -q 'Installing split from vendor/split' vendor.log || { echo "smoke test: split was not installed from vendor/" >&2; cat vendor.log >&2; exit 1; }
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
miso list
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
echo "smoke test: ok"
