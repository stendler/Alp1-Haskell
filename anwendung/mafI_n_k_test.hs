-- Haskell

-- MafI 1 Uebung 6 Selbstkontrolle und testen

--

--Hauptfunktion: berechnet hoechstmoegliche Summe von k aufeinanderfolgenden Zahlen in einer kreisfoermigen Liste mit 1 bis n zufaellig angeordneten Elementen, die in allen Kombinationsmoeglichkeiten anzutreffen ist
uebung6 :: Int -> Int -> Int
uebung6 n k = sumkMaxMin k (erzeugeListen n)

uebung6All :: Int -> Int -> [Int]
uebung6All n k = [sumkMax k ls | ls <- erzeugeListen n]

-- erzeugt eine Liste aller Kombinationsmoeglichkeiten fuer Zahlen von 1 bis n
erzeugeListen :: Int -> [[Int]]
erzeugeListen n = nextList [1,2 .. n]
  where
    delete :: Int -> [Int] -> [Int]
    delete x [] = []
    delete x (y:ys)
      | (x==y) = delete x ys
      | otherwise = y : (delete x ys)
    nextList :: [Int] -> [[Int]]
    nextList [] = [[]]
    nextList (1:list) = [1:xs | xs <- nextList list]
    nextList list = [(x:xs) | x <- list, xs <- nextList (delete x list)]

-- berechnet die Summe von k eingegebenen Zahlen
sumk :: Int -> [Int] -> Int
sumk k [] = 0
sumk k (i:is)
  | k == 0 = 0
  -- | (k > len (i:is)) = error "nicht genug Zahlen in der Liste"
  | otherwise = (i + (sumk (k-1) is))

sumGauss :: Int -> Int -> Int
sumGauss i j
  | (mod (j-i) 2)/=0 = n
  | otherwise = n-(div (j+i) 2)
  where
    n = (i+j)*(div (j-i+2) 2)

-- berechnet die _hoechste_ Summe sumk aus einer Liste mit n Zahlen
sumkMax :: Int -> [Int] -> Int
sumkMax k (i:1:is) = sumk k (i:1:is)
sumkMax k (i:is)
  | (thisSum >= followingSum) = thisSum
  | otherwise = followingSum
    where
      thisSum = sumk k (i:is)
      followingSum = sumkMax k (append i is)
      append :: Int -> [Int] -> [Int]
      append i [] = [i]
      append i (x:xs) = x : (append i xs)
      --help :: (Int,[Int],[Int],Int) -> (Int,[Int],[Int],Int)
      --help

-- berechnet die kleinste aller hoechsten Summen sumk der Listen mit n Zahlen aus der Liste aller Kombinationsmoeglichkeiten
sumkMaxMin :: Int -> [[Int]] -> Int
sumkMaxMin k [] = 0
sumkMaxMin k (l:ls)
  | (nextMax) == 0 = thisMax
  | (thisMax) <= (nextMax) = (thisMax)
  | otherwise = nextMax
  where
    thisMax = sumkMax k l
    nextMax = sumkMaxMin k ls
{-
--meine variante
varMax :: Int -> Int -> Int
varMax n k
    where
      liste = [1..(div n 2)]
      revListe = [((div n 2)+1)..1]
      append :: Int -> [Int] -> [Int]
      append i [] = [i]
      append i (x:xs) = x : (append i xs)
      delete :: Int -> [Int] -> [Int]
      delete i [] = []
      delete i (x:xs)
        | (i==x) = xs
        | otherwise = x : (delete i xs)
      addMin :: ([Int],[[Int]]) -> ([Int],[[Int]])
      addMin (l,[]) = (l,[])
      addMin (l,(y:ys)) = (l, (append (min l) y)++(snd (addMin ((delete (min l)),ys)))
      addMax :: ([Int],[[Int]]) -> ([Int],[[Int]])
      addMax (l,[]) = (l,[])
      addMax (l,(y:ys)) = (l, (append (max l) y)++(snd (addMax ((delete (max l)),ys)))
      tupls :: Int -> [[Int]] -> [[Int]]
      tupls 0 l = []
      tupls k l = append [] l
      pair :: Bool -> Int -> [Int]
      pair k l =
        | addMin
        | addMax
-}
