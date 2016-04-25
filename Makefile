all:
	stack build
install:
	stack install
pack: clean
	zip -rS ../xzaryb00_xzdene00_xrycto00.zip ../xzaryb00_xzdene00_xrycto00
clean:
	stack clean --full
	rm -rf deps\gio-0.13.1.1\dist
	rm -rf deps\glib-0.13.2.2\dist
	rm -rf deps\gtk-0.14.2\dist
	rm -rf deps\pango-0.13.1.1\dist
test:
	stack test calc
doc:
	stack haddock --no-haddock-deps
bench:
	stack bench calc
