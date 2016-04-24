all:
	stack build
install:
	stack install
pack: clean
	zip -r ../xzaryb00_xzdene00_xrycto00.zip ../xzaryb00_xzdene00_xrycto00
clean:
	rm -rf .stack-work
	rm -rf deps/gio-0.13.1.1/.stack-work
	rm -rf deps/glib-0.13.2.2/.stack-work
	rm -rf deps/gtk-0.14.2/.stack-work
	rm -rf deps/pango-0.13.1.1/.stack-work
test:
	stack test calc
doc:
	stack haddock --no-haddock-deps
bench:
	stack bench calc
