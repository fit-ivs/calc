# Kalkulačka
[![Build Status](https://travis-ci.org/fit-ivs/calc.svg?branch=master)](https://travis-ci.org/fit-ivs/calc)

Týmový projekt v předmětu IVS, FIT VUT 2016.

Jedná se o jednoduchou GUI kalkulačku, která podporuje operace +, -, *, /, !, %, log, ln, ^, e^x a závorky. Projekt je zaměřený převážně na procvičení týmové spolupráce, program samotný je téměř vedlejší. Více info k vlastnímu projektu je v [DEVELOPMENT.md](DEVELOPMENT.md).

![screenshot](https://raw.githubusercontent.com/fit-ivs/calc/master/doc/screenshot.png "Screenshot")

## Instalace
### Binární distribuce
Ve složce [bin/](https://github.com/fit-ivs/calc/tree/master/bin) je k dispozici instalační soubor pro Windows (64bit). Binární distribuce pro Linux k dispozici (zatím) není.

### Instalace ze zdroje
Pro správnou kompilaci je potřeba Stack a GTK+2. Instrukce pro instalaci Stacku jsou na [haskellstack.org](http://docs.haskellstack.org/en/stable/install_and_upgrade/). 

#### Instalace GTK+2
Windows:
Vyžaduje **aktuální** [MSYS2](http://msys2.github.io), je nutné spouštět v MinGW-w64 konzoli.
```
pacman -Sy
pacman -S mingw-w64-x86_64-pkg-config mingw-w64-x86_64-gtk2
```

Debian:
```
sudo apt-get install libgtk2.0-dev libgtk-3-dev
```

#### Kompilace
Unix:
```
cd calc/
stack setup
stack install
```

Pro Windows:
```
cd calc/
stack setup
stack --skip-msys install
```
¨
**Pozor:** na Windows je nutné do %APPDATA%/local/bin zkopírovat knihovny GTK (při instalaci MSYS2 do výchozího umístění z c:\msys64\mingw64\bin\).

## Používané nástroje
- [Haskell](https://www.haskell.org/)
- [Emacs](https://www.gnu.org/software/emacs/) ([haskell-mode](https://github.com/serras/emacs-haskell-tutorial/blob/master/tutorial.md)) nebo [Leksah](http://leksah.org/)
- [Stack](http://docs.haskellstack.org/en/stable/README/)
- [Haddock](https://wiki.haskell.org/Haddock),
- [Travis](https://travis-ci.org)
- [Trac](https://trac.edgewall.org/) ([projektový](https://trac.zarybnicky.com))
- [NSIS](http://nsis.sourceforge.net/)
- [GTK 2.0](www.gtk.org)
- [Tasty](http://documentup.com/feuerbach/tasty)
