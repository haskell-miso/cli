#!/bin/sh
# Take miso out of the tool's package database, where every project would
# use it, and keep it next to it, prebuilt: a project depends on miso (in
# its miso.yml) and locks it like any other package, at a commit of
# dmjio/miso, and miso install copies this one when it is the one locked.
#
#   mhs-VERSION/miso-default    the miso of this release: its repository,
#                               commit, version, and cabal flags
#   mhs-VERSION/miso-prebuilt/  the package, as built from them (its .pkg,
#                               data directory, and module files)
#
# scripts/build-release.sh and flake.nix run it, after installing miso.
# Usage: scripts/prebuilt-miso.sh DB REPOSITORY COMMIT FLAGS
set -eu
db=$1 repo=$2 commit=$3 flags=$4
pkg=$(cd "$db/packages" && ls | grep -E '^miso-[0-9.]+\.pkg$')
[ -n "$pkg" ] || { echo "prebuilt-miso: no miso package in $db/packages" >&2; exit 1; }
nv=${pkg%.pkg}
version=${nv#miso-}
out=$db/miso-prebuilt
rm -rf "$out"
mkdir -p "$out/packages"
# the module files that name the package (A/B.txt for module A.B)
(cd "$db" && grep -rlxF --include='*.txt' --exclude-dir=packages "$pkg" .) > "$db/.miso-modules"
(cd "$db" && tar -cf - -T .miso-modules) | (cd "$out" && tar -xf -)
(cd "$db" && xargs rm -f < .miso-modules)
# and the directories of those modules that are empty now
find "$db" -depth -type d -empty ! -path "$db/packages*" ! -path "$db/miso-prebuilt*" -exec rmdir {} \;
rm -f "$db/.miso-modules"
mv "$db/packages/$pkg" "$out/packages/"
if [ -d "$db/packages/$nv" ]; then mv "$db/packages/$nv" "$out/packages/"; fi
chmod -R u+w "$out"
printf 'repository: %s\ncommit: %s\nversion: %s\nflags: %s\n' "$repo" "$commit" "$version" "$flags" > "$db/miso-default"
