#!/bin/sh
# Unpack a release tarball and run miso docs on the examples, checking what
# the sites have: the pages and the viewer, and what the extractor found
# (docs of constructors, fields and arguments, instances, fixities, MINIMAL,
# WARNING and DEPRECATED, OPTIONS_HADDOCK, types from the type checker, and
# the Hoogle database).  Needs emcc (the viewer is built once).
# Usage: scripts/docs-test.sh miso-SYSTEM.tar.gz, or --installed for the
# miso of PATH.
set -eu
top=$(pwd)
tmp=$(mktemp -d)
if [ "$1" != --installed ]; then
  tar -C "$tmp" -xzf "$1"
  export MISO_HOME=$tmp/home PATH="$(ls -d "$tmp"/miso-*)/bin:$PATH"
fi
failed=0
has() {
  if grep -qF -- "$2" "$1"; then :; else echo "docs test: $1 does not have: $2" >&2; failed=1; fi
}
hasnt() {
  if grep -qF -- "$2" "$1"; then echo "docs test: $1 has: $2" >&2; failed=1; fi
}

cp -R "$top/examples/shapes" "$tmp/shapes"
cd "$tmp/shapes"
# a program's Main.hs at the top: not the viewer's Main, which the first
# run builds (into $MISO_HOME, for every project)
printf 'not Haskell\n' > Main.hs
miso docs
rm Main.hs
for f in index.html index.js docs.css data/_index.js shapes.txt data/Shapes.js data/Shapes.Types.js data/Shapes.Geometry.js data/Shapes.Render.js data/Shapes.Version.js; do
  [ -s "docs/$f" ] || { echo "docs test: no docs/$f" >&2; failed=1; }
done
t=docs/data/Shapes.Types.js g=docs/data/Shapes.Geometry.js r=docs/data/Shapes.Render.js
has $t '"n":"Point","a":"t:Point","k":"data"'
has $t '"n":"px","a":"v:px","k":"field"'
has $t 'the horizontal coordinate'                 # a field's -- ^ doc
has $t '"n":"Circle","a":"v:Circle","k":"constructor"'
has $t 'a circle: its centre and radius'           # a constructor's doc
has $t '"min":"bounds"'                            # MINIMAL
has $t '"k":"method"'
has $t '"dv":true'                                 # derived instances
has $t 'Bounds combine into the bounds around both' # an instance's doc
has $g '"fx":"infixl 6"'                           # infixl 6 <->
has $g '"wk":"deprecated"'                         # DEPRECATED isConvex
has $g 'the fixed point'                           # an argument's doc
has $g 'HasBounds'                                 # centroid's type, from the type checker
has $r 'the colour inside a shape'                 # a definition list
has docs/shapes.txt 'scale :: Double -> Point -> Shape -> Shape'
has docs/shapes.txt 'px :: Point -> Double'
has docs/shapes.txt 'Circle :: Point -> Length -> Shape'
has docs/shapes.txt 'version :: '                  # a type through #include "version.h"
hasnt $t 'HasField'                                # instances mhs makes for every
hasnt $t 'Typeable'                                # field and type
# again: the viewer it built, and no data of a module that is gone
touch docs/data/Gone.js
miso docs --no-types > again.log 2>&1 || { cat again.log >&2; failed=1; }
hasnt again.log 'building the viewer'
[ ! -e docs/data/Gone.js ] || { echo "docs test: docs/data/Gone.js is still there" >&2; failed=1; }

cp -R "$top/examples/edge" "$tmp/edge"
cd "$tmp/edge"
# a docs/ of the project's own is not written over
mkdir docs && echo notes > docs/notes.md
if miso docs --no-types > own.log 2>&1; then echo "docs test: miso docs wrote over docs/" >&2; failed=1; fi
has own.log 'docs/ has files miso docs did not write'
[ "$(cat docs/notes.md)" = notes ] || { echo "docs test: docs/notes.md changed" >&2; failed=1; }
rm -r docs
miso docs --no-types
e=docs/data/Edge.js
has $e '"n":"first"'
has $e '"n":"second"'                              # first, second :: t
has $e '"wk":"warning"'                            # WARNING second
has $e '"fx":"infixl 1"'
has $e '"fx":"infixr 0"'
has $e '"min":"empty, insert"'
has $e '"n":"Origin","a":"v:Origin","k":"pattern"' # bundled: Point (.., Origin)
has $e '"n":"helper"'                              # module Edge.Internal, inlined
has $e 'A block doc, with a space'                # {- | -}
has $e '[this](javascript:'                       # not a link: its scheme
has $e 'Copyright'                                 # the module header
has docs/data/_index.js '"n":"Edge.Internal"'
has docs/data/_index.js '"hidden":true'             # OPTIONS_HADDOCK hide
has docs/data/Edge.Pruned.js '"n":"shown"'
hasnt docs/data/Edge.Pruned.js '"n":"hiddenByPrune"'  # OPTIONS_HADDOCK prune
has docs/edge.txt 'pattern Origin :: Point'
has docs/data/_index.js '"n":"Index"'               # a module Index is not the index
has docs/data/Index.js '"n":"quote"'
has docs/data/Index.js "class=\\\"hl-str\\\">'\\\\''\\u003c/span>"  # '\'' is one literal
has docs/edge.txt 'quote :: Char'
if grep -qx 'noSig' docs/edge.txt; then echo "docs test: docs/edge.txt has noSig with no type" >&2; failed=1; fi

[ $failed = 0 ] || exit 1
echo "docs test: ok"
