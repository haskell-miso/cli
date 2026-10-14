#!/bin/sh
# Install the miso tool:
#
#   curl -fsSL https://haskell-miso.org/install.sh | sh
#
# Downloads the release for this OS and architecture from GitHub, checks its
# sha256, and unpacks it in $MISO_HOME (default ~/.miso).  Set MISO_VERSION
# to install a particular release (default: the latest), and
# MISO_RELEASE_URL to download from somewhere else.
set -eu

repo=${MISO_REPO:-haskell-miso/cli}
home=${MISO_HOME:-$HOME/.miso}
version=${MISO_VERSION:-latest}

if [ -t 1 ] && [ -z "${NO_COLOR:-}" ]; then
  bold=$(printf '\033[1m'); blue=$(printf '\033[34m'); green=$(printf '\033[32m')
  red=$(printf '\033[31m'); reset=$(printf '\033[0m')
else
  bold=; blue=; green=; red=; reset=
fi
say()  { printf '%s==>%s %s\n' "$bold$blue" "$reset" "$1"; }
done_() { printf '%s==>%s %s\n' "$bold$green" "$reset" "$1"; }
die()  { printf '%serror:%s %s\n' "$bold$red" "$reset" "$1" >&2; exit 1; }

case "$(uname -s)" in
  Linux)  os=linux ;;
  Darwin) os=darwin ;;
  MINGW*|MSYS*|CYGWIN*) os=windows ;;     # Windows: run this in an MSYS2 shell
  *) die "unsupported OS: $(uname -s) (miso supports Linux, macOS, and Windows with MSYS2)" ;;
esac
case "$(uname -m)" in
  x86_64|amd64)  arch=x86_64 ;;
  arm64|aarch64) arch=aarch64 ;;
  *) die "unsupported architecture: $(uname -m)" ;;
esac
system=$arch-$os
[ "$system" = aarch64-windows ] && die "there is no release for Windows on ARM yet"
asset=miso-$system.tar.gz

if [ -n "${MISO_RELEASE_URL:-}" ]; then    # a mirror, or a local directory (file://...)
  url=$MISO_RELEASE_URL
elif [ "$version" = latest ]; then
  url=https://github.com/$repo/releases/latest/download
else
  url=https://github.com/$repo/releases/download/$version
fi

command -v curl >/dev/null || die "curl is needed"
if command -v sha256sum >/dev/null; then sha="sha256sum"
elif command -v shasum >/dev/null; then sha="shasum -a 256"
else die "sha256sum or shasum is needed"; fi

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

say "Downloading miso ($version) for $system"
curl -fSL --progress-bar -o "$tmp/$asset" "$url/$asset" || die "could not download $url/$asset"
curl -fsSL -o "$tmp/$asset.sha256" "$url/$asset.sha256" || die "could not download $url/$asset.sha256"
(cd "$tmp" && $sha -c "$asset.sha256" >/dev/null) || die "checksum mismatch for $asset"

say "Installing in $home"
tar -C "$tmp" -xzf "$tmp/$asset"
mkdir -p "$home"
# A new release replaces the tool and its package database (packages from
# miso install are installed again by the next miso build).
# (Windows cannot delete a running miso.exe, as during miso upgrade, but can rename it.)
[ -f "$home/bin/miso.exe" ] && mv -f "$home/bin/miso.exe" "$home/miso.exe.old" 2>/dev/null || true
rm -rf "$home/bin" "$home"/mhs-* 2>/dev/null || true
cp -R "$tmp/miso-$system/." "$home/"

done_ "Installed $("$home/bin/miso" version)"
case ":$PATH:" in
  *":$home/bin:"*) ;;
  *) printf '\nAdd miso to your PATH:\n\n  export PATH="%s/bin:$PATH"\n\n' "$home" ;;
esac
if [ "$os" = windows ]; then
  command -v gcc >/dev/null || printf 'miso builds programs with gcc: pacman -S mingw-w64-x86_64-gcc\n'
else
  command -v cc >/dev/null || printf 'miso builds programs with a C compiler (cc); install one (Xcode command line tools, or gcc/clang).\n'
fi
