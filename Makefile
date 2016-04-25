all:
	stack build
install:
	stack install
pack: clean
	zip -r ../xzaryb00_xzdene00_xrycto00.zip ../xzaryb00_xzdene00_xrycto00
clean:
	stack clean --full
test:
	stack test calc
doc:
	stack haddock --no-haddock-deps
bench:
	stack bench calc
