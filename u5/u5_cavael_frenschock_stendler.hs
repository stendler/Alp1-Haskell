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
countCalls n = snd (fibTripleCount n)
  where
    superFibCount :: ((Int,Int,Int),Int) -> ((Int,Int,Int),Int)
    superFibCount ((a0,a1,a2),i) = ((a1,a2,a0+a1+a2),(i+1))
    fibTripleCount :: Int -> ((Int,Int,Int),Int)
    fibTripleCount n
      | n==0 = ((0,0,1), 0)
      | otherwise = (superFibCount (fibTripleCount (n-1)))

countCallsNaive :: Int -> Int
countCallsNaive n = snd (superFibNaive (n,1))
  where
    addTuple :: (Int,Int) -> (Int,Int) -> (Int,Int)
    addTuple (x1,y1) (x2,y2) = ((x1+x2),(y1+y2))
    superFibNaive :: (Int,Int) -> (Int,Int)
    superFibNaive (0,x) = (0,x)
    superFibNaive (1,x) = (1,x)
    superFibNaive (2,x) = (1,x)
    superFibNaive (n,x) = (addTuple (addTuple (superFibNaive ((n-1),(x+1))) (superFibNaive ((n-2),(x+1)))) (superFibNaive ((n-3),(x+1))))

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

--Aufgabe 3

-- deleteABs
deleteABs :: [Char] -> [Char]
deleteAbs [] = []
deleteABs ['A'] = []
deleteABs ['B'] = []
deleteABs [c] = [c]
deleteABs (c:cs)
  | (c=='A' || c=='B') = deleteABs cs
  | otherwise = c : deleteABs cs

--doubleDigits
doubleDigits :: [Char] -> [Char]
doubleDigits [] = []
doubleDigits (c:cs)
  | (ord c >=48 && ord c <= 57) = c : c : doubleDigits cs
  | otherwise = c : doubleDigits cs

--shiftDigits
shiftDigits :: [Char] -> [Char]
shiftDigits [] = []
shiftDigits (c:cs)
  | (ord c >=48 && ord c <= 56) = chr((ord c)+1) : shiftDigits cs
  | (ord c == 57) = '0' : shiftDigits cs
  | otherwise = c : shiftDigits cs

-- Aufgabe 4

-- a)
countPos :: [Int] -> Int
countPos [] = 0
countPos (n:ns)
  | (n>0) = (countPos ns)+1
  | otherwise = countPos ns

-- b)
monIncrPrefix :: [Int] -> [Int]
monIncrPrefix [] = []
monIncrPrefix [x] = [x]
monIncrPrefix (x:x2:xs)
  | (x<=x2) = x : monIncrPrefix (x2:xs)
  | otherwise = [x]

-- c)
parSum :: [Int] -> [Int]
parSum [] = []
parSum [x] = [x]
parSum (x:x2:xs) = x:(parSum ((x+x2):xs))
