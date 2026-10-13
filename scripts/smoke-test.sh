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
echo "smoke test: ok"
