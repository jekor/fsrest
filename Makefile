BIN=dirrest
DIST=dist/build/$(BIN)/$(BIN)

all : $(BIN)

install : $(DIST)
	cabal install

$(BIN) : $(DIST)
	cp $< $@

dist/setup-config : $(BIN).cabal
	cabal configure

$(DIST) : dist/setup-config $(BIN).hs
	cabal build
	@touch $@ # cabal doesn't always update the build (if it doesn't need to)
