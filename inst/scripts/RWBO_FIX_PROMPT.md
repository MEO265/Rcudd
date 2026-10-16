# Prompt: Verlustfreie Solver-Ausgabe in Rwbo

Dieser historische Übergabeprompt bleibt als Reproduktionsreferenz erhalten.
Die zusammengeführte Rcudd-Basis setzt inzwischen die korrigierte Rwbo-Ausgabe
voraus und enthält keine Reparatur beschädigter Modellzeilen mehr.

Behebe im Repository `C:\Code\Rwbo` die folgenden Probleme. Arbeite in einem
separaten Git-Worktree und erhalte die bereits vorhandenen lokalen Änderungen;
der aktuelle Arbeitsstand enthält schon Verbesserungen an Argumentquotierung,
Exit-Status-Behandlung und `timeout`. Überschreibe diese nicht.

## 1. Lange Modellzeilen unter Windows

`R/run_open_wbo.R` verwendet derzeit `system2(..., stdout = TRUE)` und verbindet
die zurückgegebenen Elemente mit `paste(output, collapse = "\n")`. Unter Windows
wird eine lange physische Ausgabezeile bei 8095 Zeichen geteilt, sogar innerhalb
eines vorzeichenbehafteten Integers. Im tatsächlich beobachteten Lauf wurde aus
`v ... 1688 -1689 1690 ...` folgende Rückgabe:

```text
v ... 1688 -168
9 1690 ...
```

Dadurch wird die zurückgegebene Variablenbelegung beschädigt, obwohl Open-WBO
korrekt `OPTIMUM FOUND` meldet. Das reproduzierende Rcudd-Syntheseproblem hat
1760 SAT-Variablen und ein Optimum von fünf; seine WCNF liegt hier:

`C:\Users\Matthias\.codex\worktrees\bdd-rwbo-minimizer\Rcudd\rwbo-output\formula.wcnf`

Korrigiere die Ursache im R-Wrapper: leite stdout in eine temporäre Datei um,
lies sie anschließend mit `readLines()` ein und entferne die Datei auch bei
Fehlern zuverlässig. Erhalte die tatsächlichen Zeilengrenzen, alle Zeichen und
vollständigen Zahlen. Eine nachträgliche Reparatur anhand einer angenommenen
Zeilenlänge ist für die Korrektur im Paket unnötig.

Beachte, dass `system2(stdout = dateipfad)` den Exit-Status direkt zurückgibt;
die bisherige Auswertung von `attr(output, "status")` muss entsprechend angepasst
werden. Bewahre die öffentliche Schnittstelle, sichtbare stderr-Diagnosen,
korrekte Argumentquotierung und die bisherige Behandlung der Statuswerte
0, 10, 20, 30, 40, 124 sowie anderer Fehler. `timeout` soll weiterhin eine echte
Begrenzung der verstrichenen Zeit sein; Open-WBO selbst unterstützt sein
`-cpu-lim`-Flag unter Windows nicht.

## 2. Separat prüfen: WCNF ohne weiche Klauseln

Mit dem CRAN-Windows-Binärpaket Rwbo 0.1.2 wurde bei `-algorithm=1` für diese
gültige, ausschließlich harte WCNF ein nativer Abbruch mit Exit-Status 5 und
leerer stdout-Ausgabe beobachtet:

```text
p wcnf 1 2 10
10 1 0
10 -1 0
```

Mit einer zusätzlichen weichen Klausel wurde korrekt UNSAT gemeldet. Reproduziere
den Fall mit dem aktuell gebauten Solver. Falls er noch besteht, behebe die
Ursache im gebündelten Open-WBO-Code für den Fall ohne weiche Klauseln. Ein
Wrapper-Workaround, der den Input stillschweigend verändert oder Exit-Status 5
als normales UNSAT interpretiert, ist keine ausreichende Korrektur. Prüfe auch
erfüllbare ausschließlich harte WCNF und gültige leere harte Klauseln. Der
Abbruch allein beweist noch nicht, welche interne Stelle ursächlich ist.
Ein konkreter, aus dem aktuellen Quelltext abgeleiteter Verdacht: Bei null
weichen Klauseln kann `MaxSAT::isBMO()` trotz leerer `orderWeights` den Wert
`true` liefern. `LinearSU::search()` kann dann `bmoSearch()` aufrufen, das
`orderWeights[0]` liest. Prüfe diesen Pfad ausdrücklich, statt nur die
Fehlermeldung im Wrapper zu unterdrücken.
Der Vergleich wurde zusätzlich mit demselben CRAN-Binärpaket bestätigt:
`-algorithm=1` liefert Exit 5, `-algorithm=1 -no-bmo` dagegen Exit 20 und
`s UNSATISFIABLE`. Das Ausschalten von BMO ist ein Diagnosehinweis; der
Standardaufruf soll ebenfalls korrekt funktionieren.

## Regressionstests und Abschluss

- Erzeuge ein kleines, schnelles MaxSAT-Problem mit etwa 5000 festgelegten
  Variablen, positiven und negativen Literalen sowie mindestens einer weichen
  Klausel. Die Modellzeile muss länger als 8095 Zeichen sein. Löse es mit dem
  echten Solver; prüfe genau eine Belegung für jede Variable, die Erfüllung
  aller harten Klauseln und den gemeldeten Zielfunktionswert. Halte diesen Test
  unabhängig von Rcudd.
  Eine solche unabhängig validierte Reproduktion ist bereits vorhanden:
  `C:\Users\Matthias\.codex\worktrees\bdd-rwbo-minimizer\Rcudd\.rwbo-test\rwbo-fix-repro\long-model.wcnf`.
  Die unveränderte Modellzeile umfasst dort 26395 Zeichen und alle 5000
  Variablen; die Wrapper-Rückgabe verändert ihre Zeilengrenzen.
- Prüfe das oben genannte reale Syntheseproblem zusätzlich, falls verfügbar.
  Seine Modellbelegung muss alle 1760 Variablen enthalten und Kosten fünf haben.
- Erhalte bzw. aktualisiere die vorhandenen Tests für kurze Ausgaben, UNSAT,
  SAT/OPTIMUM/UNKNOWN, leere Ausgabe, Pfade mit Leerzeichen und `&`, unbekannte
  Flags, fehlende Dateien, Exit-Status-Behandlung und Timeout. An die neue
  Dateiausgabe angepasste Test-Doubles sollen die Datei tatsächlich schreiben.
- Teste das Aufräumen temporärer Dateien auf Erfolgs- und Fehlerpfaden.
- Führe die relevanten Pakettests aus, aktualisiere NEWS/Dokumentation soweit
  nötig und berichte die Änderungen, Tests und verbleibenden Grenzen.
