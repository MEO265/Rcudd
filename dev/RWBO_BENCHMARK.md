# RWBO-Laufzeiten

Erneut gemessen am 17.10.2026 unter Windows 11 mit R 4.5.3 und Rwbo
0.1.2.9000. Verglichen werden zwei getrennt installierte Rcudd-Stände:

- `654c526`: alle Operatoren kosten eins.
- `9465ada`: NOT kostet eins, AND, OR und XOR kosten zwei.

Die Eingaben enthalten nur `&&`, `||` und `!`; die XORs entstehen erst durch
Optimierung. Ein XOR-Paar ist `((a && !b) || (!a && b))`. Mehrere Paare werden
mit AND verknüpft; bei neun Variablen kommt die Bedingung `i` hinzu.

## Wiederholter Vergleich

Je fünf Läufe pro Beispiel und Kostenmodell, abwechselnd AB/BA. Die Reihenfolge
der Beispiele wird in jeder zweiten Runde umgekehrt. Jeder Lauf verwendet
einen eigenen R-Prozess und einen nicht mitgemessenen Aufwärmlauf.
Beide Varianten verwenden dieselbe Rwbo-Installation, `-algorithm=1`,
zwei Millionen als Klausellimit und zehn Sekunden Zeitbudget.

Die Tabelle zeigt Mediane der Gesamtzeit des öffentlichen Rcudd-Aufrufs:
BDD, WCNF, Solver, native Prüfung und Rückübersetzung. Prozessstart,
Aufwärmlauf und die zusätzliche exhaustive Belegungsprüfung sind ausgeschlossen.

| Beispiel | Variablen | Slots | Alle Kosten 1 | NOT 1, binär 2 |
|---|---:|---:|---:|---:|
| Zwei XORs als Parität | 3 | 4 | 0,19 s | 0,19 s |
| Zwei XOR-Paare | 4 | 8 | 0,19 s | 0,19 s |
| Drei XOR-Paare | 6 | 8 | 0,63 s | 1,56 s |
| Vier XOR-Paare | 8 | 8 | 1,07 s | Zeitlimit in 5/5 Läufen |
| Vier XOR-Paare und `i` | 9 | 8 | 6,36 s | Zeitlimit in 5/5 Läufen |

Die Gesamtzeiten bei den neuen Zeitlimit-Fällen betrugen im Median 10,10 s
(acht Variablen) und 10,11 s (neun Variablen). Das sind Abbruchzeiten, keine
Lösungszeiten. Rcudd behält dann die BDD-Formel.

Bei sechs Variablen lag die reine Solverzeit bei 0,45 s gegenüber 1,39 s.
Die zusätzliche Zeit entfällt somit nahezu vollständig auf den Solver.

## Zusätzliche Läufe mit 30 Sekunden Budget

Je ein weiterer Lauf pro Variante und Beispiel, bei gleicher Slotgrenze:

| Beispiel | Alle Kosten 1 | NOT 1, binär 2 |
|---|---:|---:|
| Vier XOR-Paare, acht Variablen | 1,08 s, Optimum | Zeitlimit nach 30,11 s |
| Vier XOR-Paare und `i`, neun Variablen | 6,60 s, Optimum | Zeitlimit nach 30,18 s |

Die Verschlechterung bestätigt sich damit auch bei abwechselnder Reihenfolge
und höherem Budget. Kleine Zusatzkosten in der WCNF erlauben keine verlässliche
Laufzeitprognose. In beiden Varianten haben alle weichen Klauseln Gewicht eins.

## Kontrollen und Wiederholen

Das [Vergleichsskript](compare_rwbo_costs.R) kontrolliert das installierte
Kostenmodell anhand einer gelösten XOR-Probe. Pro Paar werden identische
BDD-Ausgangsformeln, tatsächliche Slots und SAT-Variablenzahlen geprüft.
Die neue Datei enthält genau eine zusätzliche weiche Klausel pro Slot.
Sämtliche zurückgegebenen Formeln wurden zusätzlich auf allen Belegungen
gegen die Eingabe geprüft, einschließlich der BDD-Fallbacks bei Zeitlimits.

Insgesamt: 54 Läufe, davon 42 mit bewiesenem Optimum und zwölf Zeitlimits.
`optimal` bezeichnet das Kostenminimum innerhalb der Operatorschranke.
Die [Einzelmessungen](benchmarks/rwbo-costs-2026-10-17.csv) enthalten auch
Reihenfolge, reine Solverzeit, Anzahl der SAT-Variablen und Klauseln,
Paket-Commits und Prüfsummen der verwendeten Rcudd-DLLs.

Zum Wiederholen müssen die beiden genannten Stände in getrennten
R-Bibliotheken liegen. Dann vom Rcudd-Verzeichnis aus:

```text
Rscript --vanilla dev/compare_rwbo_costs.R UNIT_LIBRARY WEIGHTED_LIBRARY OUTPUT_DIR
```

Das Skript speichert jeden Lauf und die Zusammenfassung als CSV sowie
RDS-Dateien mit Eingaben und geprüften Ergebnissen. Für Messungen nur der
aktuellen Version bleibt [benchmark_rwbo.R](benchmark_rwbo.R) verfügbar.
