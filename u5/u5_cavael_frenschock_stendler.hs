-- Uebung 05 -- Alp1: Funktionale Programmierung
-- Tutor : Ha Do
-- Bearbeiter: Jasmine Cavael, Canel Frenschock, Maximilian Stendler

import Data.Char

--Aufgabe 1
-- bisherige fib implementierungen

fibStep :: (Int,Int) -> (Int,Int)
fibStep (x,y) = (y,x+y)

fibPair :: Int -> (Int,Int)
fibPair n
  | n==0 = (0,1)
  | otherwise = fibStep (fibPair (n-1))

fastFib :: Int -> Int
fastFib n = fst (fibPair n)

--a)
superFibStep :: (Int,Int,Int) -> (Int,Int,Int)
superFibStep (a0,a1,a2) = (a1,a2,a0+a1+a2)

fibTriple :: Int -> (Int,Int,Int)
fibTriple n
  | n==0 = (0,0,1)
  | otherwise = superFibStep (fibTriple (n-1))

fsTriple :: (Int,Int,Int) -> Int
fsTriple (a,_,_) = a

superFib :: Int -> Int
superFib n = fsTriple (fibTriple n)

--b)
countCalls :: Int -> Int
countCalls n = snd (fibTripleCount n 0)
  where
    superFibCount :: ((Int,Int,Int),Int) -> ((Int,Int,Int),Int)
    superFibCount ((a0,a1,a2),i) = ((a1,a2,a0+a1+a2),i)
    fibTripleCount :: Int -> Int -> ((Int,Int,Int),Int)
    fibTripleCount n i
      | n==0 = ((0,0,1), (i+1))
      | otherwise = (superFibCount (fibTripleCount (n-1) (i+1)))



--Aufgabe 2

type Zeit = (Int,Int)

--a)
addStd :: Zeit -> Int -> Zeit
addStd (std,m) add = ((mod (std+add) 24),m)

addMin :: Zeit -> Int -> Zeit
addMin (s,m) addM
  | (addM<0) = (s,m)
  | ((m+addM)>59) = (addMin (addStd (s,m) ((m+addM)/60)) (mod (m+addM) 60))
  | otherwise = (s,m+addM)

--b)
dauerS :: Int -> Int -> Int
dauerS s0 s1
  | (s0>s1) = (24-s0+s1)
  | otherwise = s1-s0

dauerM :: Int -> Int -> Int
dauerM m0 m1
  | (m0>m1) = (60-m0+m1)
  | otherwise = m1-m0

dauer :: Zeit -> Zeit -> Zeit
dauer (s0,m0) (s1,m1) = (dauerS s0 s1,dauerM m0 m1)

--c)
anzeige :: Zeit -> [Char]
anzeige (s,m) = [chr(s+48),':',chr(m+48),' ','U','h','r']
