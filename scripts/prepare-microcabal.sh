#!/bin/sh
# Copy the MicroCabal sources to $2, renaming its Text.ParserComb (which
# MicroHs also has, with a different API) to MicroCabal.ParserComb, so the
# two can be linked into one program.  Downloads use curl instead of wget
# (macOS has no wget).
set -eu
src=$1
dst=$2
rm -rf "$dst"
mkdir -p "$dst"
cp -R "$src/src/MicroCabal" "$dst/MicroCabal"
chmod -R u+w "$dst"
cp "$src/src/Text/ParserComb.hs" "$dst/MicroCabal/ParserComb.hs"
for f in $(find "$dst" -name '*.hs'); do
  sed -e 's/^module Text\.ParserComb/module MicroCabal.ParserComb/' \
      -e 's/^import Text\.ParserComb/import MicroCabal.ParserComb/' "$f" > "$f.tmp"
  mv "$f.tmp" "$f"
done
sed -e 's|cmd env \$ "wget --quiet --output-document=" ++ fn ++ " " ++ url|cmd env $ "curl --fail --silent --show-error --location --output " ++ fn ++ " " ++ url|' \
  "$dst/MicroCabal/Unix.hs" > "$dst/MicroCabal/Unix.hs.tmp"
mv "$dst/MicroCabal/Unix.hs.tmp" "$dst/MicroCabal/Unix.hs"
grep -q 'curl --fail' "$dst/MicroCabal/Unix.hs" || { echo "prepare-microcabal: could not switch wget to curl" >&2; exit 1; }
