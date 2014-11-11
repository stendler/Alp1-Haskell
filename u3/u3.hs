{- ALP1 - Uebung03
-- Aufgabe 1
-- Tutor : Ha Do
-- Editors: Jasmine Cavael, Canel Frenschock, Maximilian Stendler
-}

--import Char
import System.Cmd
import System.Exit

--Aufgabe 1 a)
sumIncluded3 :: Int -> Int -> Int -> Bool
sumIncluded3 a b c = (a + b + c) == 2*(max c (max a b))
-- Wenn die kleineren Zahlen in der Summe die groessere ergeben, muss
-- die groessere dazu addiert (a+b+c) das doppelte der groessten Zahl ergeben ==(2*(max c (max a b)))

--Aufgabe 1 b)
sumIncluded4 :: Int -> Int -> Int -> Int -> Bool
sumIncluded4 a b c d = (sumIncluded3 a b c) || (sumIncluded3 b c d) || (sumIncluded3 c d a) || (sumIncluded3 a b d)
--

--Aufgabe 1 c)
spanOf3 :: Float -> Float -> Float -> Float
spanOf3 a b c = ((max a (max b c)) - (min a (min b c)))
--

--Aufgabe 1 d)
thirdMan :: Int -> Int -> Int
thirdMan a b
  | (((max a b) <= 3) && ((min a b) >= 1) && a/=b) = (6 - (a) - (b))
  | otherwise = 123
--

--Aufgabe 2a)
valueAt :: Float -> Float -> Float -> Float
valueAt x0 y0 x = ((-y0)/(x0))*x+y0
-- m = (-y0)/(x0)
-- n = y0
-- mx+n
-- (-y0)/(x0)*x+y0

-- Aufgabe 2b)
testParallel :: Float -> Float -> Float -> Float -> Bool
testParallel xg yg xh yh = ((-yg)/(xg)) == ((-yh)/(xh))
--

--Aufgabe 2c)
parallelThroughX :: Float -> Float -> Float -> Float
parallelThroughX xg yg xh = (-(((-yg)/(xg))*(xh)))
--

--Aufgabe 2d)
crossingAt :: Float -> Float -> Float -> Float -> Float
crossingAt xg yg xh yh
  | (testParallel xg yg xh yh) = error "Die Geraden sind parallel und haben keinen Schnittpunkt!"
  | otherwise = (yg-yh)/(((-yh)/(xh))-((-yg)/(xg)))
  -- mx+n       = ax+b  |-b
  -- mx+n-b     = ax    |-mx
  -- n-b        = ax-mx |Distributivgesetz
  -- n-b        = (a-m)x| /(a-m)
  --(n-b)/(a-m) = x
  -- x = (yg-yh)/(((-yh)/(xh))-((-yg)/(xg)))
--

--Aufgabe 2e)
computeXg :: Float -> Float -> Float -> Float
computeXg a b c
  | (a==0||b==0||c==0) = error "Input out of scope"
  | otherwise = c/a
-- ax + by = c   | y=0
-- ax = c | /a
-- x = c/a

computeYg :: Float -> Float -> Float -> Float
computeYg a b c
  | (a==0||b==0||c==0) = error "Input out of scope"
  | otherwise = c/b
-- ax + by = c   | x=0
-- by = c | /b
-- y = c/b


--Aufgabe 3 Primzahltest

--alte primtest ausm skript
primDoof :: Int -> Bool
primDoof n
  | n <= 1 = False
  | n == 2 = True
  | otherwise = help n (n-1)
  where
    help :: Int -> Int -> Bool
    help n k
      | mod n k == 0 = False
      | k == 2 && mod n k /= 0 = True
      | mod n k /= 0 = help n (k-1)

-- unsere variante
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
