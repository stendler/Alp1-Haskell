-- Uebung 04 -- Alp1: Funktionale Programmierung
-- Tutor : Ha Do
-- Bearbeiter: Jasmine Cavael, Canel Frenschock, Maximilian Stendler

import Data.Char -- Linux
--import Char --Windoof

-- Aufgabe 1: Polynome

--a) - Implementieren Sie eine Haskell-Funktion
--eval3 :: Float -> Float -> Float -> Float -> Float -> Float
--zur Auswertung von Polynomen vom Grad 3, wobei die erste Eingabestelle dem Wert r
--und die folgenden Stellen die Koeffizienten in der Reihenfolge a 3 , a 2 , a 1 , a 0 angeben.
eval3 :: Float -> Float -> Float -> Float -> Float -> Float
eval3 r a3 a2 a1 a0 = ((a3*(r^3)) + (a2*(r^2)) + (a1*r) + a0) --r=xg

{-b) Implementieren Sie eine Haskell-Funktion
nullstellenTest2 :: Float -> Float -> Float -> Bool,
die für ein durch eine Koeffizientenfolge a 2 , a 1 , a 0 gegebenes Polynom vom Grad ≤ 2
testet, ob es eine reelle Nullstelle hat.
Achtung: Hier kann a 2 auch den Wert 0 haben, dann ist der Grad kleiner als 2.-}


nullstellenTest2 :: Float -> Float -> Float -> Bool
nullstellenTest2 a2 a1 a0
  | (a2==0 && a1==0 && a0 == 0) = True            --Funktion gleich x-Achse
  | (a2==0 && a1==0) = False                      --keine Steigung und Funktion nicht x-Achse
  | (a2==0 && a1/=0) = True                       --Funktion (mx+n) hat eine Steigung --> schneidet definitiv irgendwo die x-Achse
  | otherwise = (((((a1/a2)^2)/4)-(a0/a2)) >= 0)  --
  --x1,2 = p/2 +- sprt(p²/4-q)
  --p = a1/a2
  --q = a0/a2
  --keine Nullstellen, wenn Ergebnis in der Wurzel negativ

{-c) Implementieren Sie eine Haskell-Funktion
smallestVal :: Float -> Float -> Float -> Float,
die fuer ein durch eine Koeffizientenfolge a 2 , a 1 , a 0 gegebenes Polynom vom Grad ≤ 2
den kleinsten Absolutbetrag berechnet, den das Polynom bei der Auswertung annehmen
kann.
Hinweis: Uberlegen
Sie, was das mit der Teilaufgabe b) zu tun hat.-}
smallestVal :: Float -> Float -> Float -> Float
smallestVal a2 a1 a0
  | (nullstellenTest2 a2 a1 a0) = 0 -- 0 ist der kleinstmoegliche Absolutbetrag. Wenn die Funktion min 1 Nullstelle hat, dann ist 0 das kleinstmoegliche Ergebnis
  | otherwise = abs(a0)             -- wenn keine Nullstellen, dann Abstand zur x-Achse --> a0 beschreibt die Verschiebung auf der y-Achse --> Abstand zur x-Achse

--Aufgabe 2
{-a) Sei Z(k,n) die zyklische Zahlenfolge k,k+1,k+2,... , k+n-1,k,k+1,... . Im-
plementieren Sie eine Funktion cyclicShift :: Int -> Int -> Int -> Int -> Int,
die bei einer Eingabe k n m s die Zahl m aus Z(k,n) zyklisch um s Einheiten verschiebt.
Wenn m nicht in Z(k,n) liegt, soll m ausgegeben werden.-}
cyclicShift :: Int -> Int -> Int -> Int -> Int
cyclicShift k n m s
  | (m>n || m<k) = error "m liegt ausserhalb des Intervalls" -- der Startwert ist nicht innerhalb des gegebenen Intervalls
  | otherwise = (mod (m+s-k) (n-k+1)) + k
  --modulo faengt bei 0 an
  -- -> normalisieren des Intervalls mit 0 als Intervallbeginn (-k)
  --    m um s Einheiten verschieben (m+s    -k)
  --    Intervallgrenze n - k + 1
  --    modulo hält die Verschiebung innerhalb der normalisierten Intervallgrenzen
  -- nach dem modulo renormalisieren (+k)

{-b) Implementieren Sie eine Funktion caesarForCapitals :: Char -> Int -> Char,
die bei einer Eingabe ch k das Zeichen ch um k Stellen im zyklischen Alphabet ver-
schiebt, wenn ch ein Großbuchstabe ist, anderenfalls soll ch unver ̈andert bleiben.-}
caesarForCapitals :: Char -> Int -> Char
caesarForCapitals ch k = chr(cyclicShift 65 90 (ord ch) k) -- Intervall setzen auf ASCII Code fuer Grossbuchstaben (65-90 => A-Z)

{-c) Implementieren Sie eine Funktion mirrorForCapitals :: Char -> Char -> Char,
die bei einer Eingabe ch1 ch2 von zwei Großbuchstaben das Zeichen ch1 im zyklischen
Alphabet um das Zentrum ch2 spiegelt. Ist eines der Eingabezeichen kein Großbuch-
stabe, dann soll ch1 ausgegeben werden.-}
mirrorForCapitals :: Char -> Char -> Char
mirrorForCapitals ch1 ch2
  | (((ord ch1) > 64) && ((ord ch1) < 91) && ((ord ch2) > 64) && ((ord ch2) < 91)){-ueberpruefung ob, ch1 und ch2 = Grossbuchstaben-} = chr(cyclicShift 65 90 (ord ch2) ((ord ch2)-(ord ch1))) -- verschieben innerhalb des Alphabets(65-90) vom Spiegelpunkt um Entfernung vom Startbuchstaben zum Spiegelpunkt (ch2-ch1)
  | otherwise = ch1



  ---dumme tests
  -- sleep sort
  --- --> threads starten <--
  --- --> thread sleep n
  --- --> return n
