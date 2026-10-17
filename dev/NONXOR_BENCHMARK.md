# RWBO-Laufzeiten ohne XOR in der Eingabe

Gemessen am 17.10.2026 unter Windows 11 mit R 4.5.3 und Rwbo
0.1.2.9000. **Diese Daten stammen vor der zusätzlichen Untergrenze für
binäre Operatoren.** Sie vergleichen ausschließlich die damaligen Stände:

- `654c526` (`unit`): jeder Operator kostet eins.
- `9465ada` (`weighted`): NOT kostet eins, AND, OR und XOR kosten zwei.

Alle Eingaben sind XOR-frei. Der Solver darf weiterhin XOR verwenden.
Verglichen werden jeweils 4, 6, 8 und 9 tatsächlich benötigte Variablen;
die Variablenzahl allein bestimmt die Schwierigkeit also nicht.

## Formelfamilien und Versuchsaufbau

| Familie | Beispiel mit vier Variablen | Eingabe schon optimal? |
|---|---|---|
| AND | `a && b && c && d` | Ja |
| OR | `a \|\| b \|\| c \|\| d` | Ja |
| NAND | `!(a && b && c && d)` | Ja |
| AND aus OR-Paaren (`cnf`) | `(a \|\| b) && (c \|\| d)` | Ja |
| Ausmultiplizierte DNF (`dnf`) | `(a && c) \|\| (b && c) \|\| (a && d) \|\| (b && d)` | Nein; äquivalent zur vorherigen Zeile |
| Schwellenfunktion | Mindestens zwei von `a,b,c,d`, bei mehr Variablen AND mit dem Rest | Vorher nicht bewiesen |

Bei ungerader Variablenzahl wird an das Produkt der OR-Paare noch `&& i`
angehängt. Die vier als optimal bezeichneten Familien erreichen bereits ihre
bekannten Minima: AND, OR und OR-Paarprodukte brauchen `n-1` binäre Operatoren;
NAND braucht zusätzlich eine Negation. Das gilt auch unter den neuen Kosten.
Trotzdem kann die aus dem BDD erzeugte Schreibweise länger sein als die
optimale Eingabe, sodass die automatische Schnittstelle den Solver aufruft.

Das [Skript](benchmark_nonxor.R) verwendet den
[gemeinsamen Vergleichstreiber](compare_rwbo_costs.R). Beide Versionen laufen
abwechselnd AB/BA, in getrennten R-Prozessen und getrennten R-Bibliotheken,
mit derselben Rwbo-Installation und `-algorithm=1`. Vor jeder Messung werden
Rcudd und Rwbo aufgewärmt und das installierte Kostenmodell geprüft.
Zeitbudget: zehn Sekunden; Klausellimit: zwei Millionen.

Gesamtzeit umfasst BDD, WCNF, Solver und Ergebnisinterpretation. Prozessstart,
Aufwärmen, gesonderte BDD-Baseline und zusätzliche Äquivalenzprüfungen liegen
außerhalb dieser Zeit. Ein Zeitlimit ist eine Abbruchzeit, keine Lösungszeit.

## Direkter Solveraufruf bei schon optimaler Eingabe

Hier werden die automatischen Vorauswahlen umgangen: `bdd_to_wcnf()` erzeugt
das Problem, Rwbo wird aufgerufen und sein Ergebnis interpretiert. Je drei
abwechselnde Läufe pro Formel und Kostenmodell; angegeben ist der Median der
Gesamtzeit in Sekunden. Beide Modelle erhalten dieselben tatsächlichen Slots:
`K=8`, außer NAND mit neun Variablen (`K=9`). `Limit` bedeutet Zeitlimit in
allen drei Läufen, mit Gesamtzeiten von etwa 10,14 bis 10,20 Sekunden.

| Variablen | Familie | Alle Kosten 1 | NOT 1, binär 2 |
|---:|---|---:|---:|
| 4 | AND | 0,14 | 0,16 |
| 4 | OR | 0,14 | 0,16 |
| 4 | NAND | 0,16 | 0,19 |
| 4 | AND aus OR-Paaren | 0,16 | 0,20 |
| 6 | AND | 0,28 | 1,62 |
| 6 | OR | 0,20 | 0,89 |
| 6 | NAND | 0,66 | 7,09 |
| 6 | AND aus OR-Paaren | 0,35 | 7,59 |
| 8 | AND | 0,35 | Limit |
| 8 | OR | 0,33 | Limit |
| 8 | NAND | Limit | Limit |
| 8 | AND aus OR-Paaren | 9,06 | Limit |
| 9 | AND | 0,71 | Limit |
| 9 | OR | 0,56 | Limit |
| 9 | NAND | Limit | Limit |
| 9 | AND aus OR-Paaren | Limit | Limit |

Die Verzögerung durch die neuen Kosten tritt somit auch ohne XOR-Zielfunktion
und bei bereits optimalen Eingaben auf. Bei sechs Variablen entfallen zum
Beispiel für AND aus OR-Paaren 0,24 gegenüber 7,47 Sekunden auf den Solver;
bei AND sind es 0,17 gegenüber 1,50 Sekunden. Der Hauptunterschied entsteht
im Solver. Drei Wiederholungen zeigen hier dasselbe Verhalten; sie sind
keine allgemeine Laufzeitgarantie.

## Automatische Schnittstelle für lintr

Je ein Lauf pro Eingabe und Kostenmodell durch
`bdd_restrict_from_expressions(..., use_rwbo = Inf)`. AND und OR mit 4 bis 9
Variablen werden ohne Solver als `minimal` erkannt (0,05 bis 0,07 Sekunden).
NAND wird von der Kandidatenheuristik als `not_candidate` übersprungen
(0,04 bis 0,06 Sekunden). Dieser Status ist kein Optimalitätsnachweis,
auch wenn die NAND-Eingaben in diesem Versuch bereits optimal sind.

| Variablen | Eingabe | Alle Kosten 1 | NOT 1, binär 2 |
|---:|---|---:|---:|
| 4 | AND aus OR-Paaren | 0,21 s | 0,25 s |
| 4 | Ausmultiplizierte DNF | 0,19 s | 0,21 s |
| 6 | AND aus OR-Paaren | 0,41 s | 7,66 s |
| 6 | Ausmultiplizierte DNF | 0,62 s | 1,53 s |
| 8 | AND aus OR-Paaren | 9,17 s | Limit |
| 8 | Ausmultiplizierte DNF | 1,43 s | Limit |
| 9 | AND aus OR-Paaren | Limit | Limit |
| 9 | Ausmultiplizierte DNF | Limit | Limit |

Alle abgeschlossenen Solverläufe in dieser Tabelle liefern ein verifiziertes
Optimum und verbessern die BDD-Ausgangsformel. Bei Zeitlimit bleibt die
BDD-Ausgangsformel erhalten; sie kann umfangreicher als die optimale Eingabe
sein. Gleiche Funktion und gleiche Variablenzahl garantieren außerdem nicht
dieselbe intern abgeleitete Formel oder WCNF-Struktur.

Die konfigurierte Obergrenze ist hier immer acht Slots. Automatisch werden
beim Vier-Variablen-OR-Paarprodukt im alten Modell tatsächlich sechs, im neuen
acht Slots erzeugt; sonst sind es bei allen Solveraufrufen acht.
Insbesondere diese erste Tabellenzeile isoliert daher nicht allein das
Kostenmodell. Der direkte Vergleich oben hält die tatsächlichen Slots fest.

## Zusätzliche Schwellenfunktion

Die Eingabe mit vier Variablen ist
`(a && b) || ((a || b) && (c || d)) || (c && d)`.
Bei mehr Variablen wird die ganze Funktion mit den übrigen Variablen per AND
verknüpft. Je ein direkter Lauf pro Modell, mit gleicher und hinreichend
großer Slotgrenze, um die jeweilige Eingabe darstellen zu können:

| Variablen | Slots | Alle Kosten 1 | NOT 1, binär 2 |
|---:|---:|---:|---:|
| 4 | 8 | 1,75 s, Optimum | Limit |
| 6 | 9 | Limit | Limit |
| 8 | 11 | Limit | Limit |
| 9 | 12 | Limit | Limit |

Der erfolgreiche Lauf mit vier Variablen findet sieben Operatoren, darunter
ein XOR. Die Optimalitätsangabe bezieht sich auf das Kostenminimum innerhalb
der jeweiligen Operatorschranke. Aus einem Zeitlimit folgt weder, dass eine
Eingabe optimal ist, noch dass keine bessere Formel existiert.

## Kontrollen, Rohdaten und Wiederholung

Insgesamt wurden 144 Paketaufrufe gemessen: 40 automatisch, 96 direkt mit
bereits optimalen Eingaben und acht direkte Schwellenfälle. Davon rufen 120
Rwbo auf. Es gibt 90 Optimalitätsmeldungen (einschließlich der 16 automatischen
`minimal`-Erkennungen ohne Solver), 46 Zeitlimits und acht `not_candidate`.
Es traten keine sonstigen Status oder abgebrochenen Messläufe auf.

Alle automatischen Rückgaben wurden auf sämtlichen Belegungen geprüft.
Erfolgreiche direkte Solverergebnisse wurden nativ geprüft und zusätzlich
unabhängig wieder als BDD mit der Eingabe verglichen. Die problemseitige
Wahrheitstabelle und die bekannten Operatorzahlen beziehungsweise gewichteten
Minima werden ebenfalls geprüft. Die Prüfkennzeichen bei Zeitlimits und
`not_candidate` behaupten keinen verifizierten Solvernachweis.

Ohne XOR in der Eingabe heißt nicht ohne XOR im Ergebnis: Im alten Modell
geben sechs NAND-Läufe `TRUE ^ (...)` zurück, das ebenso viele Operatoren wie
eine Negation kostet. Die neuen Kosten bevorzugen bei diesen Fällen NOT.
Dazu kommt das XOR im erfolgreichen Schwellenfall.

Die [144 Einzelmessungen](benchmarks/rwbo-nonxor-2026-10-17.csv) enthalten
Versuchsgruppe, Formelfamilie, Aufrufweg, Reihenfolge, konfigurierte und
tatsächliche Slots, Zeiten, Status, Kosten, XOR-Zahl, Paket-Commits und
DLL-Prüfsummen. `max_operators` ist die tatsächliche Slotzahl oder `NA`, wenn
kein Solverproblem erzeugt wurde. `weighted_cost` bewertet jede zurückgegebene
Formel nach NOT=1/binär=2, auch bei `unit`; es ist dort nicht der alte
Solver-Zielwert. Lange Ausdrücke und lokale Installationspfade sind weggelassen.

Die DLL-Prüfsummen waren für alle 72 Läufe je Modell konstant:
`cda16e40565dd9f2b5f8b553559523b3` (`654c526`) und
`d8991b24345dbdec57fcc7d5f55e50a8` (`9465ada`).

Zum Wiederholen die beiden genannten Stände getrennt installieren und im
Rcudd-Verzeichnis ausführen:

```text
Rscript --vanilla dev/benchmark_nonxor.R UNIT_LIBRARY WEIGHTED_LIBRARY OUTPUT_DIR
```

Die bisherigen [XOR-Messungen](RWBO_BENCHMARK.md) ergänzen diesen Vergleich.
Die Wirkung der zusätzlichen binären Untergrenze muss separat gegen diese
unveränderten Ausgangsstände gemessen werden.
