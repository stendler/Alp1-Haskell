-- Uebung 05 -- Alp1: Funktionale Programmierung
-- Tutor : Ha Do
-- Bearbeiter: Jasmine Cavael, Canel Frenschock, Maximilian Stendler

-- Aufgabe 2
{-Implementieren Sie die folgenden Funktionen mit Listenrekursion (d.h. ohne Verwen-
dung von vordefinierten Funktionen bis auf die Funktion elem)-}
ignoreDoublings, deleteRepetitions, onlySingles :: String -> String
--a)ignoreDoublings soll alle Doppel- und Mehrfachzeichen durch ein einzelnes ersetzen,
--also z.B. fuer die Eingabe "abbacxxxax" den String "abacxax" ausgeben.
ignoreDoublings [] = []
ignoreDoublings (x1:x2:xs)
  | (x1==x2) = ignoreDoublings (x2:xs)
  | otherwise = x1:ignoreDoublings (x2:xs)

{-b) deleteRepetitions soll jedes Zeichen, dass im Eingabestring vorkommt, nur einmal
in den Ausgabestring setzen also z.B. fuer die Eingabe "abbacxxxax" den String "abcx"
(oder eine Permutation dieses Strings) ausgeben.-}
deleteRepetitions

{-c) onlySingles soll nur die Zeichen in den Ausgabestring setzen, die im Eingabestring
genau einmal vorkommen, also z.B. fuer die Eingabe "abbacxxxax" den String "c" aus-
geben.-}
onlySingles

{-d) Implementieren Sie die Listenfunktion countSymbols :: String -> [(Char,Int)]
mit der fuer alle in der Liste auftretenden Symbole deren Vielfachheit bestimmt wird,
z.B. soll fuer die Eingabe "abbacxxxax" die Liste [(’a’,3),(’b’,2),(’c’,1),(’x’,4)]
ausgegeben werden.-}
countSymbols :: String -> [(Char,Int)]
countSymbols

--Aufgabe 3
{-Die folgenden Funktionen sollen mit Hilfe der ZF-Notation implementiert werden, wobei
Sie selbst herausfinden muessen, welche Hilfsfunktionen dazu benoetigt werden:
a) Die Funktion prodOf2Primes berechnet bei Eingabe n die Liste
aller Zahlen aus [4..n], deren Primproduktzerlegung aus genau zwei Faktoren besteht
(also fuer n = 14 die Liste [4,6,9,10,14]).-}
prodOf2Primes :: Int -> [Int]
prodOf2Primes

{-b) Die Funktion squareNearlyInt soll fuer einen positiven Float-
Wert x mit hoechstens einer Stelle hinter dem Komma die Liste aller Float-Werte y
erstellen, die auch hoechstens eine Stelle hinter dem Komma haben, die Ungleichung
0 ≤ y ≤ x erfuellen und deren Quadrat h ̈ochstens 0.01 von einer ganzen Zahl entfernt
ist.-}
squareNearlyInt :: Float -> [Float]
squareNearlyInt

{-c) Die Funktion mirrorCapitals extrahiert alle Großbuch-
staben aus einem String (alle anderen Zeichen werden gestrichen) und spiegelt sie danach
in der Mitte (d.h. ’A’ zu ’Z’, ’B’ zu ’Y’ usw.).-}
mirrorCapitals :: String -> String
mirrorCapitals
