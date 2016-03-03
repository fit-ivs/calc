# Kalkulačka
FIT VUT 2016, předmět IVS, projekt č. 2

## Důležité termíny
- **15. 3.** - registrace týmů
- **22. 3.** - odevzdání plánu projektu
- **3. 4.** - deadline pro program
- **10. 4.** - deadline pro dokumentaci
- **25. 4.** - odevzdání projektu (vedoucí odevzdává zip)
- **25. - 29. 4.** - registrace obhajob ve WISu
- **1. 5.** - odevzdání individuálního hodnocení projektu

## Výstupy
- [ ] repozitář včetně historie (commity minimálně každý večer!, pozor na přepisování práce ostatních, rebase vždy před push)
- [ ] Makefile (pack, clean, test, doc, [all])
- [ ] instalátor a odinstalátor
- [ ] uživatelská příručka v PDF
- [ ] programová dokumentace (Doxygen apod.)
- [ ] screenshot finální verze programu
- [ ] testy (převážně mat. knihovny)
- [ ] screenshot debuggeru/popis debugování
- [ ] info o profilování
- [ ] mockupy další verze kalkulačky
- [ ] plán práce (Gantt?) + záznam o odchylkách od něj

## Požadavky na program
- rozdělení programu na matematickou knihovnu a GUI
- základní matematické operace (+, -, \*, /), faktoriál, mocniny s přirozenými exponenty a jedna funkce navíc (třeba tetrace? :) )
- dvě možnosti instalace - pomocí (od)instalátoru (Inno, apt-get), nebo ze zdrojů ([ukázka](http://xmonad.org/intro.html))

## Instalace
```
stack build
stack exec calc-exe
```

## Vývoj
TODO - Inno/.deb distribution
```
stack test
```

### Debugování
TODO - postup ([info](https://wiki.haskell.org/Debugging))

### Profilování
TODO - postup ([wiki](https://wiki.haskell.org/Performance), [prof2dot](https://hackage.haskell.org/package/prof2dot), [Vacuum](https://thoughtpolice.github.io/vacuum/))
```
stack bench
```

### Generování dokumentace
TODO - postup (Haddock, [graphmod](https://hackage.haskell.org/package/graphmod), [hs2dot](https://hackage.haskell.org/package/hs2dot), [SourceGraph](https://hackage.haskell.org/package/SourceGraph), [graphtype](https://hackage.haskell.org/package/graphtype))
```
stack haddock
```

## Používané nástroje
- [Haskell](https://www.haskell.org/) (+ [Emacs](https://github.com/serras/emacs-haskell-tutorial/blob/master/tutorial.md) nebo [EclipseFP](https://eclipsefp.github.io/))
- [Stack](http://docs.haskellstack.org/en/stable/README/)
- [Haddock](https://wiki.haskell.org/Haddock), 
- [Travis](https://travis-ci.org) (TODO!) + [Tasty](http://documentup.com/feuerbach/tasty)
- [Github](https://github.com/) ([web](https://fit-ivs.github.io), [repozitář](https://github.com/fit-ivs/calc))
- [Trac](https://trac.edgewall.org/) ([projektový](https://trac.zarybnicky.com))
