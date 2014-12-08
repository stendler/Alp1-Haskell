-- Uebung 05 -- Alp1: Funktionale Programmierung
-- Tutor : Ha Do
-- Bearbeiter: Jasmine Cavael, Canel Frenschock, Maximilian Stendler

import Data.Char

-- Aufgabe 2
{-Implementieren Sie die folgenden Funktionen mit Listenrekursion (d.h. ohne Verwen-
dung von vordefinierten Funktionen bis auf die Funktion elem)-}
ignoreDoublings, deleteRepetitions, onlySingles :: String -> String
--a)ignoreDoublings soll alle Doppel- und Mehrfachzeichen durch ein einzelnes ersetzen,
--also z.B. fuer die Eingabe "abbacxxxax" den String "abacxax" ausgeben.
ignoreDoublings [] = []
ignoreDoublings [x] = [x]
ignoreDoublings (x1:x2:xs)
  | (x1==x2) = ignoreDoublings (x2:xs)
  | otherwise = x1:ignoreDoublings (x2:xs)

{-b) deleteRepetitions soll jedes Zeichen, dass im Eingabestring vorkommt, nur einmal
in den Ausgabestring setzen also z.B. fuer die] Eingabe "abbacxxxax" den String "abcx"
(oder eine Permutation dieses Strings) ausgeben.-}
deleteRepetitions [] = []
deleteRepetitions (c:cs)
  | (elem c cs) = deleteRepetitions cs
  | otherwise = c : deleteRepetitions cs

{-c) onlySingles soll nur die Zeichen in den Ausgabestring setzen, die im Eingabestring
genau einmal vorkommen, also z.B. fuer die Eingabe "abbacxxxax" den String "c" aus-
geben.-}
onlySingles [] = []
onlySingles [x] = [x]
onlySingles (x1:xs)
  | (elem x1 xs) = onlySingles (delete x1 xs)
  | otherwise = x1:onlySingles xs
  where
    delete :: Char -> String -> String
    delete x [] = []
    delete x (y:ys)
      | (x==y) = delete x ys
      | otherwise = y : (delete x ys)

{-d) Implementieren Sie die Listenfunktion countSymbols :: String -> [(Char,Int)]
mit der fuer alle in der Liste auftretenden Symbole deren Vielfachheit bestimmt wird,
z.B. soll fuer die Eingabe "abbacxxxax" die Liste [(’a’,3),(’b’,2),(’c’,1),(’x’,4)]
ausgegeben werden.-}
countSymbols :: String -> [(Char,Int)]
countSymbols s = tpl (s,[])
  where
    incrTplLst :: Char -> [(Char,Int)] -> [(Char,Int)]
    incrTplLst c [] = [(c,1)]
    incrTplLst c ((c2,i):cis)
      |  (c==c2) = ((c2,i+1):cis)
      | otherwise = (c2,i):incrTplLst c cis
    tpl :: (String,[(Char,Int)]) -> [(Char,Int)]
    tpl ([],l) = l
    tpl ((s:ss),l) = tpl (ss,(incrTplLst s l))

--Aufgabe 3
{-Die folgenden Funktionen sollen mit Hilfe der ZF-Notation implementiert werden, wobei
Sie selbst herausfinden muessen, welche Hilfsfunktionen dazu benoetigt werden:
a) Die Funktion prodOf2Primes berechnet bei Eingabe n die Liste
aller Zahlen aus [4..n], deren Primproduktzerlegung aus genau zwei Faktoren besteht
(also fuer n = 14 die Liste [4,6,9,10,14]).-}

-- unsere variante vom 3. uebungszettel
prim :: Int -> Bool
prim n
  | (n<=1) = False
  | (n==2) = True
  | otherwise = help 2 n
  where
    help :: Int -> Int -> Bool
    help pos n
      | (mod n pos == 0) = False
      | ((pos >= (sqrtI n)) && ((mod n pos) /=0)) = True
      | otherwise = help (pos+1) n

-- wurzel ziehen
intSqrt :: Int -> Int -> Int
intSqrt n a
  | (a*a == n) = a
  | (a*a >= n) = a-1
  | otherwise = intSqrt n (a+1)

sqrtI :: Int -> Int
sqrtI n = intSqrt n 0

-- aus 1b nur fuer Int
deleteRepetitionsInt :: [Int] -> [Int]
deleteRepetitionsInt [] = []
deleteRepetitionsInt (c:cs)
  | (elem c cs) = deleteRepetitionsInt cs
  | otherwise = c : deleteRepetitionsInt cs

--eigentliche aufgabe a:
prodOf2Primes :: Int -> [Int]
prodOf2Primes n = deleteRepetitionsInt prodList
  where
    --Liste aller Primzahlen von 2 bis div n 2
    primList = [x | x <- [2..(div n 2)], prim x]
    prodList = [x | x <- [4..n], y1 <- primList,y2 <- primList, y1*y2==x]

{-b) Die Funktion squareNearlyInt soll fuer einen positiven Float-
Wert x mit hoechstens einer Stelle hinter dem Komma die Liste aller Float-Werte y
erstellen, die auch hoechstens eine Stelle hinter dem Komma haben, die Ungleichung
0 ≤ y ≤ x erfuellen und deren Quadrat hoechstens 0.01 von einer ganzen Zahl entfernt
ist.-}
--eingabe ueberpruefen, ob eine Zahl mit max einer Stelle nach dem Komma
floatList :: Float -> [Float] -> [Float]
floatList 0 l = 0:l
floatList fl l
  | fl >= 0 = floatList (fl-0.1) (fl:l)
  | otherwise = l

squareNearlyInt :: Float -> [Float]
squareNearlyInt x = [y | y <- (floatList x []),inIntRange (y*y) (nearestInt y*y)]
  where
    nearestInt :: Float -> Float
    nearestInt f
      | f < 0.5 = 0
      | otherwise = 1 + nearestInt (f-1)
    inIntRange :: Float -> Float -> Bool
    inIntRange r near
      | (r == near) = True
      | ((r > near) && ((r - near) <= 0.01)) = True
      | ((r < near) && ((near - r) <= 0.01)) = True
      | otherwise = False

{-c) Die Funktion mirrorCapitals extrahiert alle Großbuch-
staben aus einem String (alle anderen Zeichen werden gestrichen) und spiegelt sie danach
in der Mitte (d.h. ’A’ zu ’Z’, ’B’ zu ’Y’ usw.).-}
mirrorCapitals :: String -> String
mirrorCapitals s = [chr (90 - (ord c) + 65) | c <- s,(((ord c) > 64) && ((ord c) < 91))]
