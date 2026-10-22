#!/bin/sh
# The smallest code that makes mhs crash while building safe (assertTCMode).
set -u
tarball=$1
tmp=$(mktemp -d); tar -C "$tmp" -xzf "$tarball"
export MISO_HOME=$tmp/home PATH="$(ls -d "$tmp"/miso-*)/bin:$PATH"
cd "$tmp"; miso new p > /dev/null 2>&1; cd p
printf 'name: p\nversion: 0.1.0\nmain: Main\nsource-dirs: [app]\ntarget: native\ndependencies: [safe]\n' > miso.yml
echo 'main = pure ()' > app/Main.hs
miso install > install.log 2>&1
src=$MISO_HOME/src/hackage/safe-0.3.21
echo "PROBE safe: modules: $(cd "$src" && find . -name '*.hs' | sort | tr '\n' ' ')"
for m in $(cd "$src" && find . -name '*.hs' | sed 's#^\./##'); do
  if miso mhs -i"$src" -fno-code "$src/$m" > m.log 2>&1; then echo "PROBE safe: $m ok"; else echo "PROBE safe: $m FAILS: $(grep -o 'error: .*' m.log | head -1 | cut -c1-160)"; fi
done
grep -n "TypeFamilies\|LANGUAGE\|type family\|ConstraintKinds\|Partial\|HasCallStack" "$src"/Safe/Partial.hs 2>/dev/null | head -12 | sed 's/^/PROBE safe Partial.hs: /'
