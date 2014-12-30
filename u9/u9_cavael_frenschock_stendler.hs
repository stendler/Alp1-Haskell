-- Uebung 09 -- Alp1: Funktionale Programmierung
-- Tutor : Ha Do
-- Bearbeiter: Jasmine Cavael, Canel Frenschock, Maximilian Stendler

--Aufgabe 2: Algebraische Typen I
{-Der algebraischen Datentyp Figur aus dem Skript wird so modifiziert, dass man konkrete
Kreise und Rechtecke darstellen kann: -}
data FigInPlane = Kreis Point Float | Rechteck Point Float Float
{-Dabei werden mit -}type Point =(Float,Float) {-Punkte in der Ebene dargestellt, die
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
  | (b1 < 0) = fitsInto (Rechteck (p1,q1+b1)) a1 (abs b1) (Rechteck (p2,q2) a2 b2)
  | (a2 < 0) = fitsInto (Rechteck (p1,q1) a1 b1) (Rechteck (p2+a2,q2) (abs a2) b2)
  | (b2 < 0) = fitsInto (Rechteck (p1,q1) a1 b1) (Rechteck (p2,q2+b2) a2 (abs b2))
  | ((p1>=p2) & (q1>=q2) {-& (p1<=(p2+a2)) & (q1<=(q2+b2))-} & ((p1+a1)<=(p2+a2)) & ((q1+b1)<=(q2+b2))) = True
  | otherwise = False    -- endpunkte bestimmen (p1+a1,q1+b1) --> liegen anfangs und endpunkte von darin?
fitsInto (Kreis (p1,q1) r1) (Kreis (p2,q2) r2)
  

  {-
  Rechteck (0,0)  1  1  <-- Normalform
  Rechteck (1,1) -1 -1
  Rechteck (0,1)  1 -1
  Rechteck (1,0) -1  1
  --> Alles gleiche Rechtecke
  -}
transformFitsInto :: FigInPlane -> FigInPlane -> Bool
