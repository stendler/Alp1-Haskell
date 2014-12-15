-- Uebung 08 -- Alp1: Funktionale Programmierung
-- Tutor : Ha Do
-- Bearbeiter: Jasmine Cavael, Canel Frenschock, Maximilian Stendler


--Aufgabe 1
{-a) Welche Signatur hat die Funktion length.concat? Definieren Sie eine geeignete
Funktion f, so dass man length.concat als Rechtsfaltung foldr f 0 darstellen kann.-}

--Signatur: length.concat :: [[a]] -> Int

f :: [a] -> Int -> Int
f x l = l + foldr lenF 0 x
  where
      lenF :: a -> Int -> Int
      lenF a s = s+1

lengthconcat a = foldr f 0 a

{-c) In Ubung
5, A4.c haben Sie bereits eine Funktion parSum
[ ] [2,1,4,3]
zur Partialsummenberechnung implementiert. Jetzt soll diese
[2] [1,4,3]
Aufgabe mit einer Linksfaltung realisiert werden. Auf der rechten [3,2] [4,3]
Seite sehen Sie an einem Beispiel, wie man schrittweise die umge- [7,3,2] [3]
kehrte Partialsummenfolge erzeugen kann.
[10,7,3,2] [ ]
Ueberlegen Sie sich eine geeignete Signatur und Definition einer Funktion f, so dass man
revParSum = foldl f [ ] und parSum = reverse.revParSum definieren kann.
-}

-- c)
parSum :: [Int] -> [Int]
parSum [] = []
parSum [x] = [x]
parSum (x:x2:xs) = x:(parSum ((x+x2):xs))

fl :: [Int] -> Int -> [Int]
fl [] i = [i]
fl (x:xs) i = (i+x):(x:xs)

--fr :: Int -> [Int] -> [Int]
--fr i [] = [i]
--fr i (x:xs) = (i+x):(x:xs)

revParSum :: [Int] -> [Int]
revParSum [] = []
revParSum x = foldl fl [] x

--Aufgabe 3
{-a) Die Funktion maxList :: ([Int],[Int]) -> [Int] soll fuer ein Paar von Listen
der Laengen l 1 und l 2 die Liste der Maxima an den Stellen von 0 bis min(l 1 , l 2 ) − 1
bestimmen und dahinter die restlichen Elemente der laengeren Liste anhaengen. So soll
der Aufruf maxList ([2,3,5],[4,4,4,4,1]) die Liste [4,4,5,4,1] erzeugen.-}
maxList :: ([Int],[Int]) -> [Int]
maxList ([],[]) = []
maxList ([],x) = x
maxList (x,[]) = x
maxList ((x:xs),(y:ys))
  | x >= y = x:(maxList (xs,ys))
  | y > x = y:(maxList (xs,ys))

{-b) Die Funktion mlol :: [[Int]] -> [[Int]] -> [[Int]] soll das analoge
fuer Listen von Listen leisten.-}

maxListOfLists :: [[Int]] -> [[Int]] -> [[Int]]
maxListOfLists x y
    | lenX == lenY = map maxList (zip x y)
    | lenX < lenY = map maxList (zip x y) ++ drop (lenX) y
    | lenX > lenY = map maxList (zip x y) ++ drop (lenY) x
    where
      lenX = length x
      lenY = length y

--unsere erste Variante ohne map/zip/drop

maxlol :: [[Int]] -> [[Int]] -> [[Int]]
maxlol [] [] = []
maxlol x [] = x
maxlol [] x = x
maxlol (x:xs) (y:ys) = (maxList (x,y)):(maxlol xs ys)
