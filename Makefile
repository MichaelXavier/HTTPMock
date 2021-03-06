CABAL=cabal
EXPORTS=PATH=$$PATH:cabal-dev/bin
CONFIG_OPTS=
.PHONY: test

all: build

build: configure src/**/*.hs
	$(EXPORTS) $(CABAL) build

install: build
	cabal install

uninstall:
	ghc-pkg unregister httpmock

sdist: configure
	$(CABAL) sdist
	
configure: httpmock.cabal install_dependencies
	$(CABAL) configure $(CONFIG_OPTS)

install_dependencies:
	$(CABAL) install --only-dependencies

test: configure_tests
	PATH=$$PATH:cabal-dev/bin $(CABAL) build
	$(CABAL) test --show-details=always

configure_tests:
	$(CABAL) configure --enable-tests $(CONFIG_OPTS)

docs:
	$(CABAL) haddock

clean:
	$(CABAL) clean
	rm -f **/*.{o,hi} **/**/*.{o,hi}
