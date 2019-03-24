all : build

build :
	cabal new-build

montecarlo-pi-time :
	cabal new-build montecarlo-pi --enable-tests -w ghc-7.0.4 > /dev/null
	time $$(cabal-plan list-bin montecarlo-pi)
	cabal new-build montecarlo-pi --enable-tests -w ghc-7.2.2 > /dev/null
	time $$(cabal-plan list-bin montecarlo-pi)
	cabal new-build montecarlo-pi --enable-tests -w ghc-7.4.2 > /dev/null
	time $$(cabal-plan list-bin montecarlo-pi)
	cabal new-build montecarlo-pi --enable-tests -w ghc-7.6.3 > /dev/null
	time $$(cabal-plan list-bin montecarlo-pi)
	cabal new-build montecarlo-pi --enable-tests -w ghc-7.8.4 > /dev/null
	time $$(cabal-plan list-bin montecarlo-pi)
	cabal new-build montecarlo-pi --enable-tests -w ghc-7.10.3 > /dev/null
	time $$(cabal-plan list-bin montecarlo-pi)
	cabal new-build montecarlo-pi --enable-tests -w ghc-8.0.2 > /dev/null
	time $$(cabal-plan list-bin montecarlo-pi)
	cabal new-build montecarlo-pi --enable-tests -w ghc-8.2.2 > /dev/null
	time $$(cabal-plan list-bin montecarlo-pi)
	cabal new-build montecarlo-pi --enable-tests -w ghc-8.4.4 > /dev/null
	time $$(cabal-plan list-bin montecarlo-pi)
	cabal new-build montecarlo-pi --enable-tests -w ghc-8.6.4 > /dev/null
	time $$(cabal-plan list-bin montecarlo-pi)
