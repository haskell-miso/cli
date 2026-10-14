#!/bin/sh
# Copy the MicroCabal sources to $2, renaming its Text.ParserComb (which
# MicroHs also has, with a different API) to MicroCabal.ParserComb, so the
# two can be linked into one program.  Downloads use curl instead of wget
# (macOS has no wget), and on Windows commands run with sh (from MSYS2), as
# they are Unix commands (mkdir -p, cp, tar, ...).
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
# Downloads with curl that fail on HTTP errors and follow redirects: older
# MicroCabal runs wget, newer runs "curl -s -o" (which saves an error page).
sed -e 's|cmd env \$ "wget --quiet --output-document=" ++ fn ++ " " ++ url|cmd env $ "curl --fail --silent --show-error --location --output " ++ fn ++ " " ++ url|' \
    -e 's|cmd env \$ "curl -s -o" ++ fn ++ " " ++ url|cmd env $ "curl --fail --silent --show-error --location --output " ++ fn ++ " " ++ url|' \
  "$dst/MicroCabal/Unix.hs" > "$dst/MicroCabal/Unix.hs.tmp"
mv "$dst/MicroCabal/Unix.hs.tmp" "$dst/MicroCabal/Unix.hs"
sed -e 's/^\( *\)callCommand s$/\1callCommand (viaShell s)/' \
    -e 's/^import System.Process(callCommand)$/import System.Process(callCommand)\
import qualified System.Info/' \
  "$dst/MicroCabal/Unix.hs" > "$dst/MicroCabal/Unix.hs.tmp"
mv "$dst/MicroCabal/Unix.hs.tmp" "$dst/MicroCabal/Unix.hs"
cat >> "$dst/MicroCabal/Unix.hs" <<'HS'

-- On Windows system() runs cmd.exe; run the (Unix) commands with sh.
viaShell :: String -> String
viaShell s
  | System.Info.os == "windows" = "sh -c \"" ++ concatMap (\ c -> if c == '"' then "\\\"" else [c]) s ++ "\""
  | otherwise = s
HS
grep -q 'callCommand (viaShell s)' "$dst/MicroCabal/Unix.hs" || { echo "prepare-microcabal: could not patch cmd" >&2; exit 1; }
grep -q 'curl --fail' "$dst/MicroCabal/Unix.hs" || { echo "prepare-microcabal: could not switch wget to curl" >&2; exit 1; }
