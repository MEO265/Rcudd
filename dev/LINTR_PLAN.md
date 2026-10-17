# Rcudd-Schnittstelle für lintr

Der Wiederaufnahmeplan vom 16.10.2026 ist umgesetzt. Nur Rcudd wurde geändert.
Der ursprüngliche Plan bleibt in Commit 1748de6 nachvollziehbar.

## Einstieg

`bdd_restrict_from_expressions(exprs, use_rwbo = 0L, rwbo_control = list())`
verarbeitet einen einzelnen zitierten R-Ausdruck oder eine geordnete Kette.
Ein abschließendes TRUE repräsentiert else. lintr entscheidet, welche
R-Ausdrücke für das Boolesche Modell mit totalen Vergleichsordnungen geeignet sind.

`use_rwbo` akzeptiert numerisch 0, positive ganze Variablenlimits und Inf.
Logische Werte werden abgewiesen. Variablen des Kontexts zählen mit.
Standardgrenzen: acht Operatoren, zwei Millionen Klauseln und ein gemeinsames
Budget von fünf Sekunden. Ein laufender Export und die Modellprüfung werden
abgeschlossen; Solver-Timeouts werden auf ganze Sekunden aufgerundet.

Jeder Chain-Eintrag enthält den lesbaren R-Ersatz, die rohe Boolesche Formel,
Variablenmetadaten, context_reachable, reachable und einen stabilen
optimization-Status. Details stehen in der Funktionshilfe und
[den Beispielen](../inst/scripts/README.md).

## Umsetzung und Prüfung

- Export direkt aus existierenden Ergebnis- und Kontext-BDDs.
- Kontext aus Constraints und negierten ursprünglichen Vorgängerbedingungen.
- Prüfung vollständiger Modelle, Operatoren, Kosten und kontextabhängiger
  Äquivalenz; Metadaten werden zusätzlich an Original und Kontext gebunden.
- Nur verifizierte, strikt billigere Formeln werden übernommen.
- Fehlendes Rwbo, Limits und fehlerhafte Ausgaben behalten das BDD-Ergebnis.
- Temporäre Dateien werden auch bei Fehlern entfernt.
- cpp_logic_to_r ersetzt die beiden früheren Rückkonverter.

Regressionen liegen in tests/testthat und tests/native/test_bdd_wcnf.cpp.
Reale Solver-Prüfungen: inst/scripts/test_lintr_rwbo.R und
inst/scripts/test_bdd_rwbo_minimize.R. Dazu gehören DNF-Eingaben ohne XOR,
zwei synthetisierte XORs sowie die kontextabhängige Ersetzung !a durch b.