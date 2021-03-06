# Informace k vývoji aplikace

## Důležité termíny
- **15. 3.** - registrace týmů
- **22. 3.** - odevzdání plánu projektu
- **3. 4.** - deadline pro program
- **10. 4.** - deadline pro dokumentaci
- **25. 4.** - odevzdání projektu (vedoucí odevzdává zip)
- **25. - 29. 4.** - registrace obhajob ve WISu
- **1. 5.** - odevzdání individuálního hodnocení projektu

## Výstupy
- [x] repozitář včetně historie (commity minimálně každý večer!, pozor na přepisování práce ostatních, rebase vždy před push)
- [x] Makefile (pack, clean, test, doc, [all])
- [x] instalátor a odinstalátor
- [x] uživatelská příručka v PDF
- [x] programová dokumentace (Doxygen apod.)
- [x] screenshot finální verze programu
- [x] testy (převážně mat. knihovny)
- [x] screenshot debuggeru/popis debugování
- [x] info o profilování
- [x] mockupy další verze kalkulačky
- [ ] plán práce (Gantt?) + záznam o odchylkách od něj
- [ ] vzájemné hodnocení kvality práce

## Požadavky na program
- rozdělení programu na matematickou knihovnu a GUI
- základní matematické operace (+, -, \*, /), faktoriál, mocniny s přirozenými exponenty a jedna funkce navíc (třeba tetrace? :) )
- dvě možnosti instalace - pomocí (od)instalátoru (Inno, apt-get), nebo ze zdrojů ([ukázka](http://xmonad.org/intro.html))

## Testy
Používáme doctesty (v dokumentaci jednotlivých funkcí), unit testy a QuickCheck (v src-test).
```
stack test
```

## Debugování
```
stack exec ghci
:l src/Lib.hs
:break Lib 105
evaluate (Divide (Number 1) (Number 0))
:step
:type x
:print x
:force x
evaluate x
:back
:abandon
:quit
```

## Profilování
###Benchmarky:
```
stack bench
```
###Profilování (cost centers):
```
stack install --library-profiling --executable-profiling
calc-exe +RTS -px -RTS
```
Výsledek profilování bude v souboru calc-exe.prof s údaji o časové a 'paměťové' (alokační) náročnosti.

## Generování dokumentace
```
stack haddock --no-haddock-deps
```

## Generování instalátorů
Pro vygenerování NSIS instalátoru je potřeba vygenerovat si .nsi soubor pomocí programu calc-nsis (z repozitáře, např. pomocí `stack install`), překopírovat calc-exe.exe z %APPDATA%\local\bin do src-nsis/ivs-calc.exe a knihovny .dll z instalačního adresáře MinGW64 do src-nsis/. Dále je potřeba stáhnout si [NSIS samotné](http://nsis.sourceforge.net/Main_Page) a přidat si instalační složku NSIS do PATH. Nakonec stačí spustit calc-nsis.exe v src-nsis/ a výsledkem je instalační soubor ivs-calc.exe.
