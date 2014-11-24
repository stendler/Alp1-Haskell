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

{-
teileInts :: Int -> Int -> Int
teileInts divident divisor = countDiv divident divisor 0
  where
    countDiv :: Int -> Int -> Int -> Int
    countDiv divident divisor count
      | (divident < 0) = count-1
      | (divident == 0) = count
      | otherwise = countDiv (divident-divisor) divisor count+1
-}  -- man kann auch einfach div nehmen

addMin :: Zeit -> Int -> Zeit
addMin (s,m) addM -- wir gehen laut Aufgabenstellung davon aus, dass es keine negativen Eingaben gibt
  | ((m+addM)>59) = (addMin (addStd (s,0) (div (m+addM) 60)) (mod (m+addM) 60))
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
intToString :: Int -> [Char]
intToString i
  | (i<0) = error "nur natuerliche zahlen"
  | (i<10) = [chr (0+48)]++[chr (i+48)] -- fuehrende Null
  | otherwise = addToString i []
  where
    addToString :: Int -> [Char] -> [Char]
    addToString i s
      | (i<10) = [chr (i+48)]++s
      | otherwise = addToString (div i 10) ([chr((mod i 10)+48)]++s)

anzeige :: Zeit -> [Char]
anzeige (s,m) = ((intToString s)++":"++(intToString m)++" Uhr")
