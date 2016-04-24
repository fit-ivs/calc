all:
	stack build
install:
	stack install
pack: clean
	zip -r ../calc.zip .
clean:
	rm -r .stack-work
	rm -r deps/gio-0.13.1.1/.stack-work
	rm -r deps/glib-0.13.2.2/.stack-work
	rm -r deps/gtk-0.14.2/.stack-work
	rm -r deps/pango-0.13.1.1/.stack-work
test:
	stack test calc
doc:
	stack haddock --no-haddock-deps
bench:
	stack bench calc
