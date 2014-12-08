-- Uebung 07 -- Alp1: Funktionale Programmierung
-- Tutor : Ha Do
-- Bearbeiter: Jasmine Cavael, Canel Frenschock, Maximilian Stendler

-- Aufgabe 1 - Sortieren

delMax :: [Int] -> [Int]
delMax [] = []
delMax (x:xs)
  | (x==maximum (x:xs)) = xs
  | otherwise = x : (delMax xs)

selStep :: ([Int],[Int]) -> ([Int],[Int])
selStep ([],out) = ([],out)
selStep (inp,out) = selStep (delMax inp,(maximum inp):out)

selSort :: [Int]->[Int]
selSort inp = selStep (inp,[])
