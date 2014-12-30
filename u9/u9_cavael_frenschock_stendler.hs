-- Uebung 08 -- Alp1: Funktionale Programmierung
-- Tutor : Ha Do
-- Bearbeiter: Jasmine Cavael, Canel Frenschock, Maximilian Stendler

--Aufgabe 2: Algebraische Typen I
{-Der algebraischen Datentyp Figur aus dem Skript wird so modifiziert, dass man konkrete
Kreise und Rechtecke darstellen kann: -}
data FigInPlane = Kreis Point Float | Rechteck Point Float Foalt
{-Dabei werden mit -}type Point =(Float,Float) {-Punkte in der Ebene dargestellt, die
jeweils den Mittelpunkt der Figur festlegen. Darueber hinaus soll Rechteck p a b ein
achsenparalleles Rechteck mit horizontaler Ausdehnung a und vertikaler Ausdehnung b
repraesentieren.
Implementieren Sie fitsInto, transformFitsInto::FigInPlane->FigInPlane->Bool
als Funktionen, die fuer zwei Figuren entscheiden, ob die erste in der zweiten enthalten
ist, bzw. ob man die erste so drehen und verschieben kann, dass sie danach in der
zweiten enthalten ist.-}

fitsInto :: FigInPlane -> FigInPlane -> Bool

transformFitsInto :: FigInPlane -> FigInPlane -> Bool
