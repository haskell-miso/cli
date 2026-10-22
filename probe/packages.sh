#!/bin/sh
# Install one Hackage package in a new native project and use it:
#   probe/packages.sh TARBALL PACKAGE MODULE EXPRESSION
# Prints "PROBE ok: ..." or "PROBE FAIL: ..." and the end of the log.
set -u
tarball=$1 pkg=$2 mod=$3 expr=$4
tmp=$(mktemp -d)
tar -C "$tmp" -xzf "$tarball"
export MISO_HOME=$tmp/home PATH="$(ls -d "$tmp"/miso-*)/bin:$PATH"
cd "$tmp"
miso new p > /dev/null 2>&1
cd p
cat > miso.yml <<YML
name: p
version: 0.1.0
main: Main
source-dirs: [app]
target: native
dependencies: [$pkg]
YML
cat > app/Main.hs <<HS
module Main where
import qualified $mod as M
main :: IO ()
main = print ($expr)
HS
start=$(date +%s)
if timeout 1500 miso run > run.log 2>&1; then
  echo "PROBE ok: $pkg ($(( $(date +%s) - start ))s): $(tail -1 run.log | tr -d '\r' | cut -c1-80)"
else
  echo "PROBE FAIL: $pkg ($(( $(date +%s) - start ))s)"
  grep -v '^\s*$' run.log | tail -12 | cut -c1-220
fi
