# RWBO-Laufzeiten

Gemessen am 17.10.2026 auf diesem Rechner, R 4.5.3 und Rwbo 0.1.2.9000.
Median aus drei Aufrufen je Fall; R-Prozessstart und nachträgliche exhaustive
Kontrolle sind ausgeschlossen. Die Gesamtzeit umfasst den öffentlichen
Rcudd-Aufruf mit BDD, WCNF, Solver, Prüfung und Rückübersetzung.

| Variablen | Zielformel | Max. Operatoren | Gesamt | Davon Solver | Ergebnis |
|---:|---|---:|---:|---:|---|
| 4 | 2 XOR-Paare mit AND | 8 | 0,17 s | 0,02 s | Optimum: 3 Operatoren |
| 6 | 3 XOR-Paare mit AND | 8 | 0,63 s | 0,45 s | Optimum: 5 Operatoren |
| 8 | 4 XOR-Paare mit AND | 8 | 1,08 s | 0,89 s | Optimum: 7 Operatoren |
| 9 | 4 XOR-Paare mit AND, zusätzlich i | 8 | 6,55 s | 6,33 s | Optimum: 8 Operatoren |
| 10 | 5 XOR-Paare mit AND | 9 | 10,20 s | 10,09 s | Zeitlimit: BDD-Fallback |

Zeitbudget: zehn Sekunden pro Aufruf; Klausellimit: unverändert zwei Millionen.
Für zehn Variablen wurde die Operatorschranke auf neun erhöht, damit die bekannte
neun Operatoren lange Lösung überhaupt im Suchraum liegt. Der reine BDD-Weg
ohne RWBO dauerte bei diesen Fällen jeweils etwa 0,05 bis 0,06 Sekunden.

Das Neun-Variablen-Beispiel erreicht mit dem Standardbudget von fünf Sekunden
das Zeitlimit (gemessen 5,19 s) und behält die BDD-Formel. Mit zehn Sekunden
Budget wurde das Optimum in allen drei Läufen bewiesen.

Die Struktur spielt ebenfalls eine Rolle: Eine reine Parität über acht
Variablen brauchte in einem zusätzlichen Einzelversuch nur 0,50 s, während
vier XOR-Paare mit AND etwa eine Sekunde brauchten. Diese Werte sind Beispiele,
keine allgemeine Laufzeitgrenze.

## Wiederholen

Diese Rcudd-Version und die gefixte Rwbo-Version müssen installiert sein:

```r
source("dev/benchmark_rwbo.R")
zeiten <- benchmark_rwbo() # fünf Fälle, je drei Läufe, maximal zehn Sekunden
```

Der Input enthält kein xor(), sondern pro Paar dieses ausgeschriebene Muster:
`((a && !b) || (!a && b))`. Die Muster werden mit AND verknüpft; bei ungerader
Variablenzahl kommt eine weitere Bedingung hinzu.

```r
formel <- rwbo_pair_example(9L)
ergebnis <- Rcudd::bdd_restrict_from_expressions(
  formel, use_rwbo = Inf, rwbo_control = list(time_limit = 10L)
)$chain[[1L]]
ergebnis$reduced_expression
# z.B. i && xor(g, h) && xor(c, d) && xor(e, f) && xor(a, b)
```

Das Benchmarkscript misst den Solver durch einen temporären Trace innerhalb
dieses R-Prozesses. Es prüft jede zurückgegebene Formel zusätzlich auf allen
Belegungen und entfernt den Trace beim Verlassen der Funktion.