# Wiederaufnahme: Rcudd-Schnittstelle für lintr

## Gesicherter Stand

- Worktree: `C:/Users/Matthias/.codex/worktrees/bdd-rwbo-minimizer/Rcudd`
- Branch: `codex/rcudd-rwbo-foundation`
- Bereinigung: Commit `b446a7b`. Paketinstallation, testthat und die Integration
  mit der installierten, gefixten Rwbo-Version waren erfolgreich.
- Unfertiger nativer Kontext-Encoder liegt im Stash mit dem Namen
  `WIP lintr interface: contextual WCNF and model verification`.
  Er umfasst drei Dateien und wurde noch nicht getestet oder an R angebunden.
- Nur Rcudd wird geändert. Änderungen in lintr und Rwbo gehören nicht dazu.

## Ziel und öffentlicher Vertrag

lintr soll ausschließlich `bdd_restrict_from_expressions()` benötigen:

```r
# Einzelner Ausdruck
bdd_restrict_from_expressions(list(quote(a && b)), use_rwbo = 6L)

# if / else if / else, in Quelltextreihenfolge
bdd_restrict_from_expressions(
  list(quote(a > b), quote(a >= b), TRUE),
  use_rwbo = 6L
)
```

Ein einzelnes Element wird ohne vorherige Bedingungen vereinfacht. Mehrere
Elemente teilen Platzhalter und Vergleichsregeln. Ein abschließendes TRUE steht
für else. lintr entscheidet, ob die ursprünglichen R-Ausdrücke für klassische
Boolesche Logik und das Modell einer totalen Ordnung geeignet sind.

`use_rwbo` ist ausschließlich numerisch: 0 deaktiviert, eine positive ganze
Zahl begrenzt die Variablenzahl, Inf entfernt diesen Benutzerfilter.
Logische Werte einschließlich TRUE und FALSE sowie NA, NaN, negative,
gebrochene und nichtskalare Werte werden abgewiesen. Die unabhängigen
Ressourcenlimits bleiben auch bei Inf bestehen.

## 1. Kontext-Encoder abschließen

1. Stash über seinen Namen identifizieren, Diff prüfen und gezielt anwenden.
2. Die native Überladung für ein bestehendes BDD plus Care-BDD fertigstellen.
   Der Kontext enthält zusätzliche Constraints UND die Negationen aller
   vorherigen ursprünglichen Bedingungen.
3. Nur relevante Care-Zeilen erzwingen. Unter Kontext darf die Schranke
   n - 1 nicht aus der bloßen Anzahl verfügbarer Variablen abgeleitet werden.
4. Variablen aus Ergebnis und Kontext gemeinsam zählen. Pool und Bedeutung
   des bewiesenen Minimums dokumentieren.
5. Care-Metadaten durch R-Exporter und Interpreter reichen. Überprüfen:
   care & (original XOR kandidat) ist FALSE.
6. Tests für Konstanten, leere Kontexte, neue Kontextvariablen, reorderte BDDs
   und Formeln, die nur im Kontext äquivalent sind.

Commit: Kontextabhängiger Export und verifizierte Interpretation.

## 2. Optionale Solver-Steuerung ergänzen

1. `use_rwbo = 0L` und optional `rwbo_control = list()` an den bestehenden
   Literal-, Chain- und R-Ausdrucksfunktionen ergänzen. Ein gemeinsamer Validator.
2. Kontrollwerte für Operatoren, Klauseln und ein gemeinsames Zeitbudget pro
   Aufruf vorsehen. Vorschlag für Defaults: acht Operatoren, zwei Millionen
   Klauseln und fünf Sekunden insgesamt. Der native Variablen-Hardcap bleibt.
3. Konstanten und nachweislich minimale Formeln überspringen. Wiederholte
   Variablen als Kandidatenheuristik verwenden; der Wegfall einer Variable
   allein ist kein Anlass für einen Solver-Aufruf.
4. WCNF in C++ erzeugen, solange Ergebnis- und Kontext-BDDs leben.
   Keine BDD-Formel für den Export erneut parsen.
5. R ruft `Rwbo::run_open_wbo(timeout = ...)` auf und interpretiert das
   vollständige Modell. Temporäre Dateien auch bei Fehlern entfernen.
6. Nur nachweislich äquivalente und strikt billigere Kandidaten übernehmen.
   Fehlendes Rwbo, Ressourcenlimits, Timeout und ungültige Modelle behalten
   das BDD-Ergebnis mit einer Diagnose bei. Falsche API-Argumente sind Fehler.
7. Kandidatenprüfung und Optimalitätsbeweis getrennt ausweisen.

Commit: Begrenzte automatische RWBO-Optimierung.

## 3. Rückgabe für lintr vervollständigen

Pro Chain-Eintrag eindeutig dokumentieren und liefern:

- `reduced_expression`: lesbarer R-Ersatz; `reduced_cpp_expression`: Rohform.
- `constant`, `variables`, `initial_variables`: zur ausgegebenen Formel
  passende Informationen; ursprüngliche Eingabevariablen bleiben erhalten.
- `context_reachable`: der Kontext vor Prüfung der Bedingung ist erfüllbar.
- `reachable`: Kontext UND Bedingung sind erfüllbar; unterscheidet einen
  unerreichbaren Zweig von einer erreichbaren, stets falschen Prüfung.
- `optimization`: stabiler Status, Übernahme ja/nein, Kosten vorher/nachher,
  Verifikation und Optimalität. Auch bei deaktiviertem Solver konsistent.

Die Kontextfortschreibung verwendet stets die ursprünglichen Bedingungen.
Ein anderer außerhalb des Kontexts gewählter Kandidat darf spätere Zweige
nicht verändern. Vergleichstokens und XOR müssen wieder korrekt in R-Syntax
übersetzt werden. lintr muss keine Dateien oder Solver-Ausgaben verwalten.

`cpp_logic_to_r()` ist der gemeinsame vektorisierte Rückkonverter. Die zwei
alten Namen können entfallen: Namensänderungen wurden ausdrücklich erlaubt.
Die übrigen niedrigeren APIs nur behalten, wo sie einen eigenen Zweck erfüllen.

Commit: Einheitlicher Rcudd-Einstieg für einzelne Bedingungen und Chains.

## 4. Abschließende Prüfung

- Unveränderte Ergebnisse bei deaktiviertem RWBO; strenge Argumentvalidierung.
- Tatsächlich installierten gefixten Rwbo verwenden.
- DNF ohne XOR ergibt drei Variablen mit zwei XORs; außerdem zwei XORs plus AND.
- Kontextabhängige Verbesserung, unerreichbares else und Vergleichsketten.
- Fehler- und Timeout-Fallback, gemeinsames Budget und Dateibereinigung.
- Native Regression, testthat, reale Solver-Integration und R CMD check.
- Kurze Doku mit einem Einzel- und einem Chain-Beispiel; keine lintr-Änderungen.

## Lokale Ausführung

R 4.5.3 unter `C:/Program Files/R/R-4.5.3/bin/x64`, Testbibliothek
`.rwbo-test/library`. Rwbo ist global installiert. Für Windows-Checks
`LC_ALL=C` und `LANG=C` setzen. Rtools45 benötigt sowohl
`x86_64-w64-mingw32.static.posix/bin` als auch `usr/bin` im PATH.
Zusätzliche lokale Details stehen in `.rwbo-test/lintr-handoff.md`.