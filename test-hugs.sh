#!/bin/sh

set -e

CABAL=${CABAL:-cabal}
HC=${HC:-ghc}

# Install cpphs if it is not in path
command -v cpphs || ${CABAL} v2-install --ignore-project --with-compiler "$HC" cpphs

# Regenerate splitmix-hugs
sh make-hugs.sh
find splitmix-hugs

# Simple test
echo 'nextWord64 (mkSMGen 42)' | hugs -98 -P:splitmix-hugs -p'> ' System.Random.SplitMix | tee hugs.output
grep '(1275548033995301424,SMGen 4530528345362647137 13679457532755275413)' hugs.output
