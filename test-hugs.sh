#!/bin/sh

set -e

CABAL=${CABAL:-cabal}
HC=${HC:-ghc}

# Install cpphs if it is not in path
command -v cpphs || ${CABAL} v2-install --ignore-project --with-compiler "$HC" cpphs

# Regenerate splitmix-hugs
sh make-hugs.sh
find splitmix-hugs

die() {
    echo "TEST FAILED"
    exit 1
}

dotest() {
  echo "$2" | hugs -98 -P:splitmix-hugs -p'> ' "$1" | tee hugs.output
  grep "$3" hugs.output || die
}

# Simple tests
dotest System.Random.SplitMix   'nextInteger (-100) 73786976294838206464 (mkSMGen 42)' '(10417309031967932979,SMGen 18209985878117922550 13679457532755275413)'
dotest System.Random.SplitMix32 'nextInteger (-100) 73786976294838206464 (mkSMGen 42)' '(63481308251723623759,SMGen 2735861347 1604540297)'
