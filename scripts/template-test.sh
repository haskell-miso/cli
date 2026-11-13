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

# miso dev: serves the page, builds, and builds again when a source changes
(
  cd counter-dev
  NO_COLOR=1 miso dev --port 8797 > dev.log 2>&1 &
  pid=$!
  trap 'kill $pid 2>/dev/null || true' EXIT
  # wait up to ten minutes for the Nth "Built" line
  built() {
    i=0
    while [ "$(grep -c '^==> Built counter-dev' dev.log)" -lt "$1" ]; do
      i=$((i + 1))
      [ $i -le 600 ] || fail "counter-dev: miso dev did not build (build $1)" dev.log
      kill -0 $pid 2>/dev/null || fail "counter-dev: miso dev stopped" dev.log
      sleep 1
    done
  }
  built 1
  page=$(curl -s http://127.0.0.1:8797/ || true)
  case "$page" in *'<script'*) ;; *) fail "counter-dev: miso dev did not serve the page: $page" dev.log ;; esac
  [ -s public/app.comb ] || fail "counter-dev: miso dev made no public/app.comb" dev.log
  sleep 1
  printf '\n-- changed\n' >> app/Main.hs
  built 2
  grep -q '^==> Changed: .*Main.hs' dev.log || fail "counter-dev: miso dev did not say what changed" dev.log
  echo "template test: counter-dev ok (miso dev)"
)

# a dev build with the runtime built already needs no emcc
(
  cd counter-dev
  PATH=$(printf '%s\n' "$PATH" | tr ':' '\n' | while read -r d; do [ -x "$d/emcc" ] || printf '%s:' "$d"; done)
  ! command -v emcc > /dev/null || fail "counter-dev: emcc is still on the PATH"
  miso build --dev counter-dev > build.log 2>&1 || fail "counter-dev: miso build --dev needed emcc with the runtime built" build.log
  [ -s public/index.js ] || fail "counter-dev: miso build --dev without emcc made no public/index.js" build.log
  echo "template test: counter-dev ok (miso build --dev, without emcc)"
)
echo "template test: ok"
