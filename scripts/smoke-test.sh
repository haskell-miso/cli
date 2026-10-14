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
