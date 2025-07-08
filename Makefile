all : build

build :
	cabal v2-build

build-ghcjs :
	cabal v2-build -w /opt/ghcjs/8.4/bin/ghcjs --ghcjs

montecarlo-pi-time :
	cabal v2-build montecarlo-pi --enable-tests -w ghc-7.0.4 > /dev/null
	time $$(cabal-plan list-bin montecarlo-pi)
	cabal v2-build montecarlo-pi --enable-tests -w ghc-7.2.2 > /dev/null
	time $$(cabal-plan list-bin montecarlo-pi)
	cabal v2-build montecarlo-pi --enable-tests -w ghc-7.4.2 > /dev/null
	time $$(cabal-plan list-bin montecarlo-pi)
	cabal v2-build montecarlo-pi --enable-tests -w ghc-7.6.3 > /dev/null
	time $$(cabal-plan list-bin montecarlo-pi)
	cabal v2-build montecarlo-pi --enable-tests -w ghc-7.8.4 > /dev/null
	time $$(cabal-plan list-bin montecarlo-pi)
	cabal v2-build montecarlo-pi --enable-tests -w ghc-7.10.3 > /dev/null
	time $$(cabal-plan list-bin montecarlo-pi)
	cabal v2-build montecarlo-pi --enable-tests -w ghc-8.0.2 > /dev/null
	time $$(cabal-plan list-bin montecarlo-pi)
	cabal v2-build montecarlo-pi --enable-tests -w ghc-8.2.2 > /dev/null
	time $$(cabal-plan list-bin montecarlo-pi)
	cabal v2-build montecarlo-pi --enable-tests -w ghc-8.4.4 > /dev/null
	time $$(cabal-plan list-bin montecarlo-pi)
	cabal v2-build montecarlo-pi --enable-tests -w ghc-8.6.5 > /dev/null
	time $$(cabal-plan list-bin montecarlo-pi)

generate-mix32 :
	cabal v2-build generate-mix32 && $$(cabal-plan list-bin generate-mix32)

doctest :
	perl -i -e 'while (<ARGV>) { print unless /package-id base-compat-\d+(\.\d+)*/; }' .ghc.environment.*
	doctest src

native:
	nix-build -A native.splitmix

freebsd:
	nix-build -A freebsd.splitmix

android:
	nix-build -A android.splitmix

js:
	nix-build -A js.splitmix

wasm:
	nix shell \
	  'gitlab:haskell-wasm/ghc-wasm-meta/7927129e42bcd6a54b9e06e26455803fa4878261?host=gitlab.haskell.org' \
	  --command sh -c "wasm32-wasi-cabal update && wasm32-wasi-cabal test all --ghc-options='-single-threaded' --test-wrapper=wasmtime"
