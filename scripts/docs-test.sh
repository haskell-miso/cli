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
miso docs
for f in index.html index.js docs.css data/index.js shapes.txt data/Shapes.js data/Shapes.Types.js data/Shapes.Geometry.js data/Shapes.Render.js; do
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

cp -R "$top/examples/edge" "$tmp/edge"
cd "$tmp/edge"
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
has $e 'Copyright'                                 # the module header
has docs/data/index.js '"n":"Edge.Internal"'
has docs/data/index.js '"hidden":true'             # OPTIONS_HADDOCK hide
has docs/data/Edge.Pruned.js '"n":"shown"'
hasnt docs/data/Edge.Pruned.js '"n":"hiddenByPrune"'  # OPTIONS_HADDOCK prune
has docs/edge.txt 'pattern Origin :: Point'

[ $failed = 0 ] || exit 1
echo "docs test: ok"
