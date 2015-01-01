-- Uebung 09 -- Alp1: Funktionale Programmierung
-- Tutor : Ha Do
-- Bearbeiter: Jasmine Cavael, Canel Frenschock, Maximilian Stendler

import Data.Char

--Aufgabe 2: Algebraische Typen I
{-Der algebraischen Datentyp Figur aus dem Skript wird so modifiziert, dass man konkrete
Kreise und Rechtecke darstellen kann: -}
type Point =(Float,Float)
data FigInPlane = Kreis Point Float | Rechteck Point Float Float
{-Dabei werden mit type Point =(Float,Float) Punkte in der Ebene dargestellt, die
jeweils den Mittelpunkt der Figur festlegen. Darueber hinaus soll Rechteck p a b ein
achsenparalleles Rechteck mit horizontaler Ausdehnung a und vertikaler Ausdehnung b
repraesentieren.
Implementieren Sie fitsInto, transformFitsInto::FigInPlane->FigInPlane->Bool
als Funktionen, die fuer zwei Figuren entscheiden, ob die erste in der zweiten enthalten
ist, bzw. ob man die erste so drehen und verschieben kann, dass sie danach in der
zweiten enthalten ist.-}

fitsInto :: FigInPlane -> FigInPlane -> Bool
fitsInto (Rechteck (p1,q1) a1 b1) (Rechteck (p2,q2) a2 b2)
  | (a1 < 0) = fitsInto (Rechteck (p1+a1,q1) (abs a1) b1) (Rechteck (p2,q2) a2 b2)
  | (b1 < 0) = fitsInto (Rechteck (p1,q1+b1) a1 (abs b1)) (Rechteck (p2,q2) a2 b2)
  | (a2 < 0) = fitsInto (Rechteck (p1,q1) a1 b1) (Rechteck (p2+a2,q2) (abs a2) b2)
  | (b2 < 0) = fitsInto (Rechteck (p1,q1) a1 b1) (Rechteck (p2,q2+b2) a2 (abs b2))
  | ((p1>=p2) && (q1>=q2) {-& (p1<=(p2+a2)) & (q1<=(q2+b2))-} && ((p1+a1)<=(p2+a2)) && ((q1+b1)<=(q2+b2))) = True
  | otherwise = False    -- endpunkte bestimmen (p1+a1,q1+b1) --> liegen anfangs und endpunkte von darin?
fitsInto (Kreis (p1,q1) r1) (Kreis (p2,q2) r2)
  | (r1 > r2) = False
  | (r1+(sqrt (((max p1 p2) - (min p1 p2))^2 + ((max q1 q2) - (min q1 q2))^2))) <= r2 = True
  | otherwise = False
fitsInto (Kreis (p1,q1) r) rechteck = fitsInto (Rechteck (p1-r,q1-r) (2*r) (2*r)) rechteck
fitsInto (Rechteck (x,y) a b) (Kreis (p2,q2) r) -- alle 4 eckpunkte des rechtecks muessen auf oder innerhalb des kreises sein
  | (dInC (x,y)) && (dInC (x+a,y)) && (dInC (x,y+b)) && (dInC (x+a,y+b)) = True
  | otherwise = False
  where
    dInC :: Point -> Bool
    dInC (p1,q1)
      | (sqrt (((max p1 p2) - (min p1 p2))^2 + ((max q1 q2) - (min q1 q2))^2)) <= r = True
      | otherwise = False
  {-
  Rechteck (0,0)  1  1  <-- Normalform
  Rechteck (1,1) -1 -1
  Rechteck (0,1)  1 -1
  Rechteck (1,0) -1  1
  --> Alles gleiche Rechtecke

  r1 > r2 = False
  r1 == r2 & p1 == p2 & q1 == q2 = True
  Liegt (p1,q1) innerhalb (Kreis (p2,q2) (r2-r1)) ?

  -}
transformFitsInto :: FigInPlane -> FigInPlane -> Bool
transformFitsInto (Kreis _ r1) (Kreis _ r2)
  |  r1 <= r2 = True
  | otherwise = False
transformFitsInto (Kreis _ r) (Rechteck _ a b) = fitsInto (Kreis (a/2,b/2) r) (Rechteck (0,0) a b)
transformFitsInto (Rechteck _ a b) (Kreis _ r) = fitsInto (Rechteck ((-a/2),(-b/2)) a b) (Kreis (0,0) r)
transformFitsInto (Rechteck _ a1 b1) (Rechteck _ a2 b2) = fitsInto (Rechteck (0,0) (min (abs a1) (abs b1)) (max (abs a1) (abs b1))) (Rechteck (0,0) (min (abs a2) (abs b2)) (max (abs a2) (abs b2)))

--Aufgabe 3 - Algebraische Typen II
{-Implementieren Sie fuer den im Vorlesungsskript definierten Datentyp Palindrom die fol-
genden Funktionen, die direkt, also ohne Nutzung der Funktion palToString definiert
werden sollen:
palLength :: Palindrom->Int fuer die Laenge des Strings,
checkIntegral :: Palindrom->Bool ueberprueft, ob der String eine ganze Zahl darstellt,
palToInt :: Palindrom->Int bestimmt (falls moeglich) die durch den String darge-
stellte ganze Zahl und erzeugt sonst eine Fehlermeldung,
numberOfChanges :: Palindrom->Int zaehlt die Anzahl der Symbolwechsel beim
Lesen des Strings, also z.B. 2 fuer das Palindrom aabbbaa und 4 fuer das Palindrom
abbabba. Beachten Sie dabei, dass der String nicht erzeugt werden soll, sondern die
Funktion nur mit den Konstruktoren des Datentyps Palindrom realisiert werden muss.-}
data Palindrom = Empty | Single Char | Compose Char Palindrom

palLength :: Palindrom -> Int
palLength Empty = 0
palLength (Single c) = 1
palLength (Compose c p) = 2+(palLength p)

checkIntegral :: Palindrom -> Bool
checkIntegral Empty = False
checkIntegral (Single c)
  | ((ord c) >= 48) && (ord c <= 57) = True
  | otherwise = False
checkIntegral (Compose c Empty) = checkIntegral (Single c)
checkIntegral (Compose c p) = checkIntegral (Single c) && checkIntegral p

palToInt :: Palindrom -> Int
palToInt p
  | checkIntegral p = toIntHelp p
  | otherwise = error "Palindrom ist keine Zahl!"
  where
    toIntHelp :: Palindrom -> Int
    toIntHelp (Single c) = ord c - 48
    toIntHelp (Compose c Empty) = num*10+num
    where
      num = palToInt (Single c)
    toIntHelp (Compose c p) = num*10^((palLength (Compose c p))-1) + (palToInt p)*10 + num
    where
      num = palToInt (Single c)
