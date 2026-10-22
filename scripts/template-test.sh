#!/bin/sh
# Build every template for the browser with emcc, as a new user would:
# miso new, then miso build for target: browser (WebAssembly) and
# browser_js (JavaScript), and the counter as miso dev builds it (miso build
# --dev: combinators, run by a runtime built with emcc).  The smoke test
# builds native programs only, so without this a release could ship
# templates that do not build for the browser.
#
#   scripts/template-test.sh miso-SYSTEM.tar.gz      (emcc on the PATH)
set -eu
tarball=$1
tmp=$(mktemp -d)
tar -C "$tmp" -xzf "$tarball"
export MISO_HOME=$tmp/home PATH="$(ls -d "$tmp"/miso-*)/bin:$PATH"
miso version
emcc --version | head -1
cd "$tmp"

fail() { echo "template test: $1" >&2; [ -f "${2:-}" ] && cat "$2" >&2; exit 1; }

for template in $(miso templates | awk '$1 != "==>" { print $1 }'); do
  for target in browser browser_js; do
    name=$template-$(echo "$target" | tr _ -)
    miso new "$name" --template "$template" > /dev/null
    (
      cd "$name"
      sed "s/^target: browser\$/target: $target/" miso.yml > miso.yml.new
      mv miso.yml.new miso.yml
      grep -q "^target: $target\$" miso.yml || fail "$name: miso.yml has no target: $target" miso.yml
      miso build > build.log 2>&1 || fail "$name: miso build failed" build.log
      [ -s public/index.js ] || fail "$name: miso build made no public/index.js" build.log
      [ -s public/index.html ] || fail "$name: miso build made no public/index.html" build.log
      echo "template test: $name ok ($(du -sh public | cut -f1))"
    )
  done
done

# miso dev's build: the program as combinators, and the runtime that runs them
miso new counter-dev > /dev/null
(
  cd counter-dev
  miso build --dev counter-dev > build.log 2>&1 || fail "counter-dev: miso build --dev failed" build.log
  [ -s public/app.comb ] || fail "counter-dev: miso build --dev made no public/app.comb" build.log
  [ -s public/index.js ] || fail "counter-dev: miso build --dev made no public/index.js" build.log
  echo "template test: counter-dev ok (miso build --dev)"
)
echo "template test: ok"
