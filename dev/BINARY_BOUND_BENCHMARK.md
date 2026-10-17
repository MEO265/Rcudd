# Laufzeitvergleich mit expliziter binärer Untergrenze

Gemessen am 17.10.2026 unter Windows 11 mit R 4.5.3 und Rwbo 0.1.2.9000.
**Die zusätzliche Untergrenze hilft bei mehreren Beispielen deutlich; die
großen XOR-Beispiele erreichen weiterhin das Zeitlimit.** Verglichen werden:

- Vorher: `9465ada`, in den Rohdaten `weighted`.
- Nachher: `dfb39e0`, in den Rohdaten `bounded`.

Beide Versionen verwenden dieselben Kosten: NOT=1, AND/OR/XOR=2.
Das ist ein eigener Vergleich zur binären Untergrenze. Der frühere
[Vergleich gleicher und unterschiedlicher Operatorkosten](NONXOR_BENCHMARK.md)
untersucht eine andere Änderung.

## Änderung und unverändertes Optimierungsziel

Ohne einschränkenden Kontext benötigt jeder Formelbaum für `n` tatsächlich
benötigte Variablen mindestens `n-1` binäre Operatoren. Eine Negation verbindet
keine zusätzlichen Eingänge. Die neue harte Bedingung macht genau diese
notwendige Eigenschaft explizit; die bisherigen Bedingungen an Wahrheitstabelle,
Formelbaum und Kosten bleiben erhalten.

Die [Kodierung](../src/binary_lower_bound.h) verwendet bei Bedarf Hilfsvariablen
für Präfixzähler: Ein gesetzter Zeuge für mindestens `j` binäre Operatoren in
den ersten `i` Slots muss durch frühere Zeugen und die Operatortypen gestützt
sein. Jede bisher gültige Formel lässt sich mit ihren tatsächlichen
Präfixanzahlen zu einer gültigen Zählerbelegung erweitern. Umgekehrt kann ein
Zeuge keine größere Zahl behaupten, als wirklich vorhanden ist.

Nach Ausblenden der neuen Hilfsvariablen bleiben somit dieselben gültigen
Formeln und Kosten erhalten. Eine andere gleich gute Schreibweise ist
möglich. Die Bedingung gilt nur bei uneingeschränkter Äquivalenz; bei einem
tatsächlich einschränkenden Care-Kontext wird sie nicht verwendet.
Zusätzliche Variablen und Klauseln werden bei den Ressourcenlimits mitgezählt.

## Messverfahren

Das [neue Skript](benchmark_binary_bound.R) verwendet den
[gemeinsamen Vergleichstreiber](compare_rwbo_costs.R), dieselbe Rwbo-Installation,
`-algorithm=1`, zehn Sekunden Zeitbudget und zwei Millionen als Klausellimit.
Die Versionen werden in getrennten Bibliotheken und R-Prozessen geladen,
abwechselnd AB/BA ausgeführt und vor der Messung aufgewärmt. Die Reihenfolge
der Fälle wird in jeder zweiten Wiederholung umgekehrt.

Pro Paar sind Eingabe, BDD-Ausgangsformel, konfigurierte und tatsächlich
erzeugte Slots identisch. Die erwarteten zusätzlichen SAT-Variablen und
Klauseln werden anhand der Zählerdimensionen geprüft. Beide Varianten
verwenden weiterhin zwei weiche Klauseln mit Gewicht eins je Slot.

Gesamtzeit umfasst BDD, WCNF, Solver und Interpretation. Prozessstart,
Aufwärmen, eine separat gemessene BDD-Baseline sowie zusätzliche
Äquivalenzprüfungen liegen außerhalb dieser Zeit. `Limit` bedeutet
Zeitlimit und ist keine Lösungszeit; die Gesamtzeit enthält etwas Aufwand
für Erzeugung und Abbruch. In diesen Messungen griff kein Klausellimit.

## Direkter Vergleich bei gleichen Slots

Je drei Läufe pro Fall und Version. Mediane in Sekunden; bei `Limit` liefen
alle drei Wiederholungen in das Zeitlimit. Die Solver-Spalten geben auch in
diesen Fällen die gemessene Abbruchzeit an.

Die XOR-Eingaben sind AND-verknüpfte XOR-Paare, die im Input als AND/OR/NOT
aufgeschrieben sind; bei neun Variablen kommt `&& i` hinzu. AND, OR, NAND
und AND aus OR-Paaren sind schon optimal aufgeschriebene Eingaben. Der
direkte Aufruf umgeht die automatische Kandidatenauswahl bewusst.

| Fall | Variablen | Slots | Gesamt vorher | Gesamt nachher | Solver vorher | Solver nachher |
|---|---:|---:|---:|---:|---:|---:|
| Zwei XOR-Paare | 4 | 8 | 0,16 | 0,16 | 0,03 | 0,03 |
| Drei XOR-Paare | 6 | 8 | 1,52 | 0,46 | 1,39 | 0,34 |
| Vier XOR-Paare | 8 | 8 | Limit | Limit | 10,11 | 10,02 |
| Vier XOR-Paare und `i` | 9 | 8 | Limit | Limit | 10,05 | 10,05 |
| AND | 8 | 8 | Limit | 1,38 | 10,05 | 1,21 |
| OR | 8 | 8 | Limit | 0,36 | 10,11 | 0,21 |
| AND aus OR-Paaren | 6 | 8 | 7,60 | 1,91 | 7,48 | 1,78 |
| AND aus OR-Paaren | 8 | 8 | Limit | Limit | 10,13 | 10,05 |
| NAND | 6 | 8 | 7,17 | 1,28 | 7,05 | 1,15 |
| NAND | 9 | 9 | Limit | Limit | 10,05 | 10,03 |

Bei den drei Fällen, die beide Versionen abschließen und bei denen sich
mehr als die Messauflösung ändert, sinkt die Gesamtzeit um etwa Faktor 3,3
(XOR-Paare mit sechs Variablen), 4,0 (OR-Paarprodukt mit sechs Variablen)
und 5,6 (NAND mit sechs Variablen). AND und OR mit acht Variablen bekommen
nun innerhalb des Budgets einen Optimalitätsnachweis. Der Unterschied
entsteht überwiegend im Solver.

Alle abgeschlossenen Fälle liefern die erwarteten Operatorzahlen und Kosten.
Dass eine Eingabe bereits optimal ist, erspart einem direkten Solveraufruf
weder das Finden einer gültigen Belegung noch den Nachweis der Optimalität.
Die neue Untergrenze erleichtert diesen Nachweis, ersetzt die verbleibende
Suche nach einer passenden Formelstruktur aber nicht. Aus den Zeitlimits
lässt sich keine Aussage über längere, hier nicht gemessene Laufzeiten
ableiten.

## Automatische Schnittstelle

Je ein Lauf pro Fall und Version durch
`bdd_restrict_from_expressions(..., use_rwbo = Inf)`:

| Fall | Vorher | Nachher | Ergebnis |
|---|---:|---:|---|
| AND, acht Variablen | 0,05 s | 0,06 s | Beide `minimal`, kein Solver |
| OR, acht Variablen | 0,06 s | 0,05 s | Beide `minimal`, kein Solver |
| NAND, neun Variablen | 0,06 s | 0,06 s | Beide `not_candidate`, kein Solver |
| AND aus OR-Paaren, sechs Variablen | 7,68 s | 1,97 s | Beide verbessert, Optimum |
| AND aus OR-Paaren, acht Variablen | Limit | Limit | BDD-Rückgabe |
| Ausmultipliziertes OR-Paarprodukt, acht Variablen | Limit | 2,88 s | Nachher verbessert, Optimum |
| Vier XOR-Paare, acht Variablen | Limit | Limit | BDD-Rückgabe |
| Vier XOR-Paare und `i` | Limit | Limit | BDD-Rückgabe |

Bei allen automatischen Solveraufrufen sind es auf beiden Seiten tatsächlich
acht Slots. Das ausmultiplizierte OR-Paarprodukt benötigt nachher 2,66 Sekunden
reine Solverzeit. CNF und ausmultiplizierte DNF beschreiben dieselbe Funktion,
werden aber durch die Vergabe der Platzhalter nach ihrem ersten Auftreten
unterschiedlich nummeriert. Das ergibt hier verschiedene Suchaufgaben und
erklärt die unterschiedlichen Laufzeiten dieser beiden Eingaben.

Die Kandidatenauswahl bleibt unverändert: `minimal` ist eine
direkte Erkennung ohne Solver; `not_candidate` ist eine heuristische Auswahl
und kein Solvernachweis. Bei Zeitlimit bleibt die BDD-Ausgangsformel erhalten.
Diese Schreibweise kann länger als eine schon optimale Eingabe sein.

Die automatischen Fälle hier haben keinen einschränkenden Kontext. Für
if/else-Ketten wird die neue Untergrenze nicht pauschal auf die Variablen des
Kontexts übertragen. Die Messwerte zeigen daher keine Beschleunigung für
beliebige spätere, kontextbeschränkte Zweige.

## Explorative Schwellenfälle

Je ein zusätzlicher direkter Lauf: mindestens zwei von `a,b,c,d`, bei acht
Variablen AND mit `e,f,g,h`. Die Eingabe ist ohne XOR geschrieben; ihre
Optimalität wird nicht vorausgesetzt. Beide Varianten erreichen bei vier
Variablen mit acht Slots und bei acht Variablen mit elf Slots das Zeitlimit.
Auch hierfür ist kein Gewinn innerhalb des gewählten Budgets nachgewiesen.
Die auf Timeout zurückgegebene Eingabe ist kein vom Solver bewiesenes Optimum.

## Vollständigkeit und Wiederholung

80 gemessene Paketaufrufe: 60 direkt, 16 automatisch und vier explorative
Schwellenfälle. Davon rufen 74 den Solver auf. Je Version sind es 40 Läufe:

| Status | Vorher | Nachher |
|---|---:|---:|
| Optimum mit Solvernachweis | 13 | 20 |
| `minimal`, ohne Solver | 2 | 2 |
| Zeitlimit | 24 | 17 |
| `not_candidate`, ohne Solver | 1 | 1 |

In allen 15 beidseitig als optimal erkannten Messpaaren stimmen gewichtete
Kosten und Operatorzahlen exakt überein; zwei dieser Paare sind die
automatischen Minimalitätserkennungen ohne Solver. Für alle 37 Solverpaare
stimmen die zusätzlichen SAT-Variablen und Klauseln mit der erwarteten
Zählerkodierung überein.

Alle erfolgreichen direkten Solverergebnisse sind nativ verifiziert und
zusätzlich unabhängig als BDD mit der Eingabe verglichen. Automatische
Rückgaben werden auf sämtlichen Belegungen ausgewertet, einschließlich der
BDD-Rückgaben nach Zeitlimit. Die ursprünglichen Wahrheitstabellen und die
bekannten Kostenminima werden ebenfalls geprüft. Es gab keine anderen
Status oder unvollständigen Messpaare. Das ist eine Kontrolle dieser Fälle,
keine allgemeine Garantie für bessere Laufzeiten.

Die [80 Einzelmessungen](benchmarks/rwbo-binary-bound-2026-10-17.csv) enthalten
Versuchsgruppe, Familie, Aufrufweg, Wiederholung, Reihenfolge, beide Zeiten,
Status, Operatorzahl, gewichtete Kosten, tatsächliche Slots und die neuen
Felder `binary_lower_bound` und `binary_auxiliaries`. Angaben zur erwarteten
binären Untergrenze sind nur bei `solver_called=TRUE` Teil einer tatsächlich
erzeugten WCNF. Bei ausgelassenem Solveraufruf ist die tatsächliche Slotzahl
`NA`. Lange Formeln und lokale Installationspfade sind weggelassen.

DLL-Prüfsummen für je 40 Läufe:
`d8991b24345dbdec57fcc7d5f55e50a8` (`9465ada`) und
`12bd24ee85cb18728f9805ed21f53369` (`dfb39e0`). Die native Änderung steht in
[Commit dfb39e0](https://github.com/MEO265/Rcudd/commit/dfb39e0).

Zusätzlich bestehen native Tests, R-Tests und reale Rwbo-Integrationsprüfungen.
Ein vollständiger Quellpaketbau und `R CMD check --no-manual` einschließlich
Vignetten-Neubau schließen mit `Status: OK` ab.

Zum Wiederholen die beiden Stände getrennt installieren und im
Rcudd-Verzeichnis aufrufen:

```text
Rscript --vanilla dev/benchmark_binary_bound.R BEFORE_LIBRARY AFTER_LIBRARY OUTPUT_DIR AFTER_COMMIT
```
