#!/bin/sh
# Ship the source of one of the tool's libraries in its package database:
# mhs-VERSION/miso-sources/NAME-VERSION.tar.gz (and NAME-VERSION.flags, the
# cabal flags it was built with).  A project that replaces a package of the
# tool (a dependency with git:, path:, or another version:) has miso build
# the tool's libraries that need it again, from these, against it.  The
# directory is the source as the package was built from it (patched, if it
# was).  scripts/build-release.sh and flake.nix run it.
#
# Usage: scripts/ship-source.sh DB NAME DIR [FLAGS]
set -eu
db=$1 name=$2 dir=$3 flags=${4:-}
nv=$(cd "$db/packages" && ls | grep -E "^$name-[0-9.]+\.pkg$" | sed 's/\.pkg$//')
[ -n "$nv" ] || { echo "ship-source: no package $name in $db/packages" >&2; exit 1; }
tmp=$(mktemp -d)
mkdir "$tmp/$nv"
(cd "$dir" && tar -cf - --exclude=.git --exclude=dist-mcabal .) | tar -xf - -C "$tmp/$nv"
mkdir -p "$db/miso-sources"
(cd "$tmp" && tar -czf "$db/miso-sources/$nv.tar.gz" "$nv")
if [ -n "$flags" ]; then printf '%s\n' "$flags" > "$db/miso-sources/$nv.flags"; fi
rm -rf "$tmp"
