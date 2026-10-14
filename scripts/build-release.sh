#!/bin/sh
# Build a release tarball without nix (Intel macOS, and Windows with MSYS2),
# doing what flake.nix does with the miso flake:
#
#   1. MicroHs, at the revision the miso flake pins, built and installed
#      in a fresh package database
#   2. the packages miso needs under mhs, and miso itself, installed in it
#      (as miso's nix/mhs/default.nix does)
#   3. the miso tool, built with that mhs
#   4. miso-SYSTEM.tar.gz (bin/miso and the package database), and its sha256
#
# The revisions come from the miso input of flake.lock, so the two builds
# stay in step.  Usage: scripts/build-release.sh SYSTEM   (e.g. x86_64-darwin)
set -eu

system=$1
top=$(pwd)
work=$top/build/release
rm -rf "$work"
mkdir -p "$work/src"

case "$system" in
  *-windows) windows=1; exe=.exe ;;
  *) windows=; exe= ;;
esac

say() { printf '==> %s\n' "$1"; }

# Download a GitHub repository at a revision, or a tarball, into $work/src/NAME.
fetch() {
  name=$1 url=$2
  say "fetching $name"
  mkdir -p "$work/src/$name"
  curl -fsSL -o "$work/$name.tar.gz" "$url"
  if [ -n "$windows" ]; then
    # MSYS2 makes a symlink by copying its target, which fails when the target
    # comes later in the archive; on the second pass the targets are there.
    tar -xzf "$work/$name.tar.gz" -C "$work/src/$name" --strip-components=1 2>/dev/null || true
  fi
  tar -xzf "$work/$name.tar.gz" -C "$work/src/$name" --strip-components=1
  rm -f "$work/$name.tar.gz"
}
github() { fetch "$1" "https://github.com/$2/$3/archive/$4.tar.gz"; }

# The miso and MicroCabal revisions of flake.lock.
lockrev() {
  awk -v n="$1" '
    $0 ~ "^    \"" n "\": \\{" { node = 1 }
    node && /"locked"/ { locked = 1 }
    node && locked && /"rev":/ { gsub(/[",]/, "", $2); print $2; exit }' flake.lock
}
misorev=$(lockrev miso)
mcabalrev=$(lockrev microcabal)
for r in "$misorev" "$mcabalrev"; do
  echo "$r" | grep -Eq '^[0-9a-f]{40}$' || { echo "bad revision in flake.lock: $r" >&2; exit 1; }
done
github miso dmjio miso "$misorev"
github microcabal haskell-miso MicroCabal "$mcabalrev"

# The pins of miso's nix/source.nix: NAME OWNER REPO REV, or NAME URL.
pins=$(awk '
  /^ *[a-z-]+ = fetch(FromGitHub|zip) *\{/ { name = $1; owner = repo = rev = url = "" }
  /owner *=/ { owner = $3 } /repo *=/ { repo = $3 } /rev *=/ { rev = $3 } /url *=/ { url = $3 }
  /^ *\};/ && name != "" {
    gsub(/[";]/, "", owner); gsub(/[";]/, "", repo); gsub(/[";]/, "", rev); gsub(/[";]/, "", url)
    if (url != "") print name, url; else print name, owner, repo, rev
    name = ""
  }' "$work/src/miso/nix/source.nix")
pin() {
  echo "$pins" | awk -v n="$1" '$1 == n { $1 = ""; print substr($0, 2) }'
}
for p in microhs mhs-ghc-compat mhs-array mhs-containers; do
  set -- $(pin $p)
  [ $# -eq 3 ] || { echo "no pin for $p in miso's nix/source.nix" >&2; exit 1; }
  github "$p" "$1" "$2" "$3"
done
for p in mhs-transformers mhs-mtl; do
  fetch "$p" "$(pin $p)"
done

# 1. MicroHs
say "building MicroHs"
prefix=$work/prefix
mhssrc=$work/src/microhs
# On Windows this runs in MSYS2's POSIX environment (msystem MSYS), where
# MicroHs builds with its unix configuration (its mingw one has bit-rotted).
conf=unix
(cd "$mhssrc" && make CONF=$conf bin/mhs bin/cpphs bin/mcabal)
export CABALDIR=$prefix HOME=$work/home
mkdir -p "$HOME"
(cd "$mhssrc" && make CONF=$conf MCABAL="$prefix" install)
version=$(cd "$prefix" && ls -d mhs-* | head -1 | sed 's/^mhs-//')
db=$prefix/mhs-$version
# mcabal runs mhs from the PATH
export PATH="$prefix/bin:$PATH"
mhs=$prefix/bin/mhs$exe
# mhs finds the database next to it (bin/../mhs-VERSION); naming it with -a
# too would make every package ambiguous
mcabal="$prefix/bin/mcabal$exe"


# 2. The packages, as miso's nix/mhs/default.nix installs them
inst() {
  say "installing $1"
  rm -rf "$work/pkg"
  cp -R "$work/src/$1" "$work/pkg"
  (cd "$work/pkg$2" && $mcabal -q install)
}
inst mhs-ghc-compat ""
inst mhs-array ""
inst mhs-transformers ""
# mtl 2.3.1: MicroHs cannot parse the poly-kinded instance head
sed -e 's/^instance forall k (r :: k) (m :: (k -> Type)) \. MonadCont (ContT r m) where/instance MonadCont (ContT r m) where/' \
  "$work/src/mhs-mtl/Control/Monad/Cont/Class.hs" > "$work/cont.hs"
mv "$work/cont.hs" "$work/src/mhs-mtl/Control/Monad/Cont/Class.hs"
inst mhs-mtl ""
# containers depends on template-haskell, only for Lift instances that are not compiled
mkdir -p "$work/src/template-haskell"
cat > "$work/src/template-haskell/template-haskell.cabal" <<CABAL
cabal-version:      2.4
name:               template-haskell
version:            2.22.0.0
synopsis:           Empty stand-in for template-haskell when building with MicroHs
build-type:         Simple

library
    default-language: Haskell2010
    build-depends:    base
CABAL
inst template-haskell ""
inst mhs-containers /containers
say "installing miso"
rm -rf "$work/pkg"
cp -R "$work/src/miso" "$work/pkg"
(cd "$work/pkg" && $mcabal -q -f"-template-haskell -aeson -native -production -ssr -benchmark -text" install)

# 3. The miso tool
say "building miso"
ldflags=
make MHS="$mhs" MHSSRC="$mhssrc" MCABALSRC="$work/src/microcabal" CONF=$conf EXE=$exe LDFLAGS="$ldflags"

# 4. The tarball
say "packing miso-$system"
out=$top/build/out
name=miso-$system
rm -rf "$out"
mkdir -p "$out/$name/bin"
cp "build/miso$exe" "build/cpphs$exe" "$out/$name/bin/"
cp -R "$db" "$out/$name/"
rm -f "$out/$name/mhs-$version/miso-installed"
# what comes with the tool (miso list tells these from what users install)
(cd "$out/$name/mhs-$version/packages" && ls *.pkg) > "$out/$name/mhs-$version/miso-bundled"
(cd "$out" && tar -czf "$name.tar.gz" "$name" && rm -rf "$name")
if command -v sha256sum >/dev/null; then
  (cd "$out" && sha256sum "$name.tar.gz" > "$name.tar.gz.sha256")
else
  (cd "$out" && shasum -a 256 "$name.tar.gz" > "$name.tar.gz.sha256")
fi
say "built $out/$name.tar.gz"
