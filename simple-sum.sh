# shellcheck disable=SC2086
set -ex

GHC=ghc-8.4.4
GHCJS=/opt/ghcjs/8.4/bin/ghcjs

CLOSURE_OPTS="--compilation_level=SIMPLE --isolation_mode=IIFE --assume_function_wrapper --jscomp_off=*"

# BUILD
mkdir -p dist-newstyle

cabal v2-build -O2 simple-sum -w $GHC
SIMPLE_SUM_GHC=$(cabal-plan list-bin simple-sum)

cabal v2-build -O2 simple-sum -w $GHCJS --ghcjs
SIMPLE_SUM_GHCJS=dist-newstyle/simple-sum.js
cp $(cabal-plan list-bin simple-sum).jsexe/all.js $SIMPLE_SUM_GHCJS
cp $(cabal-plan list-bin simple-sum).jsexe/all.js.externs $SIMPLE_SUM_GHCJS.externs

SIMPLE_SUM_GHCJS_CLOSURE=dist-newstyle/simple-sum-closure.js
time java -jar /opt/closure-compiler/closure-compiler.jar $CLOSURE_OPTS \
    --js "$SIMPLE_SUM_GHCJS" \
    --externs "$SIMPLE_SUM_GHCJS".externs \
    --js_output_file $SIMPLE_SUM_GHCJS_CLOSURE

cabal v2-build -O2 simple-sum -w $GHCJS --ghcjs --constraint="splitmix +optimised-mixer"
SIMPLE_SUM_GHCJS_OPT=dist-newstyle/simple-sum-optimised.js
cp $(cabal-plan list-bin simple-sum).jsexe/all.js $SIMPLE_SUM_GHCJS_OPT
cp $(cabal-plan list-bin simple-sum).jsexe/all.js.externs $SIMPLE_SUM_GHCJS_OPT.externs

SIMPLE_SUM_GHCJS_OPT_CLOSURE=dist-newstyle/simple-sum-optimised-closure.js
time java -jar /opt/closure-compiler/closure-compiler.jar $CLOSURE_OPTS \
    --js "$SIMPLE_SUM_GHCJS_OPT" \
    --externs "$SIMPLE_SUM_GHCJS_OPT".externs \
    --js_output_file $SIMPLE_SUM_GHCJS_OPT_CLOSURE

# FILESIZES
ls -l \
    $SIMPLE_SUM_GHC \
    $SIMPLE_SUM_GHCJS \
    $SIMPLE_SUM_GHCJS_CLOSURE \
    $SIMPLE_SUM_GHCJS_OPT \
    $SIMPLE_SUM_GHCJS_OPT_CLOSURE

# RUN
time $SIMPLE_SUM_GHC splitmix
time $SIMPLE_SUM_GHC splitmix32
time $SIMPLE_SUM_GHC random

time node $SIMPLE_SUM_GHCJS splitmix
time node $SIMPLE_SUM_GHCJS splitmix32
# time node $SIMPLE_SUM_GHCJS random

# optimised: only splitmix32
time node $SIMPLE_SUM_GHCJS_CLOSURE

# manual optimised
time node $SIMPLE_SUM_GHCJS_OPT
time node $SIMPLE_SUM_GHCJS_OPT_CLOSURE
