all : dirrest

configure :
	cabal configure

dirrest : dirrest.hs
	cabal build

install : dist/build/dirrest/dirrest
	cp $< .