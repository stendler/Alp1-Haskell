Uebung 11 -- Alp1: Funktionale Programmierung
Tutor : Ha Do
Bearbeiter: Jasmine Cavael, Canel Frenschock, Maximilian Stendler

> import Data.String

Aufgabe 1: Binaersuche

Implementieren den Datentyp SearchTree als echten Binaerbaum in dem jeder Knoten
einen Int-Wert als Schluessel hat und jedes Blatt zusaetzlich einen String beinhaltet.

> data SearchTree = Blatt Int String | Knoten Int SearchTree SearchTree

a) Implementieren Sie eine Funktion checkSTProp :: SearchTree -> Bool, die ueber-
prueft, ob der Baum mit den gegebenen Schluesseln ein binaerer Suchbaum ist, und eine
Funktion search :: Int -> SearchTree -> Bool, die (unter Voraussetzung der Such-
baumeigenschaft) mit Binaersuche feststellt, ob der uebergebene Suchwert als Schluessel
im Baum vorkommt.

> checkSTProp :: SearchTree -> Bool
> checkSTProp (Blatt i _) = True
> checkSTProp (Knoten i s1 s2) = checkLeft i s1 && checkRight i s2
>   where
>     checkLeft :: Int -> SearchTree -> Bool
>     checkLeft v (Blatt i _)
>       | (i <= v) = True
>       | otherwise = False
>     checkLeft v (Knoten i s1 s2)
>       | (i <= v) = checkLeft i s1 && checkLeftRight v i s2
>       | otherwise = False
>     checkLeftRight :: Int -> Int -> SearchTree -> Bool
>     checkLeftRight v i (Blatt j _)
>       | ((j >= i) && (j <= v)) = True
>       | otherwise = False
>     checkLeftRight v i (Knoten j s1 s2)
>       | ((j >= i) && (j <= v)) = checkLeft j s1 && checkLeftRight v j s2
>       | otherwise = False
>     checkRight :: Int -> SearchTree -> Bool
>     checkRight v (Blatt i _)
>       | (i >= v) = True
>       | otherwise = False
>     checkRight v (Knoten i s1 s2)
>       | (i >= v) = checkLeft i s1 && checkRight i s2
>       | otherwise = False

-- urspruengliche Variante
checkSTProp (Knoten v (Blatt i1 _) (Blatt i2 _))
 | ((i1 <= v) && (i2 >= v)) = True
 | otherwise = False
checkSTProp (Knoten v (Knoten i1 s1 s2) (Blatt i2 _))
 | ((i1 <= v) && (i2 >= v)) = checkSTProp (Knoten i1 s1 s2)
 | otherwise = False
checkSTProp (Knoten v (Blatt i1 _) (Knoten i2 s1 s2))
 | ((i1 <= v) && (i2 >= v)) = checkSTProp (Knoten i2 s1 s2)
 | otherwise = False
checkSTProp (Knoten v (Knoten i1 s1 s2) (Knoten i2 s3 s4))
 | ((i1 <= v) && (i2 >= v)) = checkSTProp (Knoten i1 s1 s2) && checkSTProp (Knoten i2 s3 s4)
 | otherwise = False

> search :: Int -> SearchTree -> Bool
> search i (Blatt v _)
>   | (i == v) = True
>   | otherwise = False
> search i (Knoten v s1 s2)
>   | (i == v) = True
>   | (i < v) = search i s1
>   | (i > v) = search i s2

b) Ab jetzt setzen wir voraus, dass alle als SearchTree verwendeten Objekte die Such-
baumeigenschaft haben.
Implementieren Sie eine Funktion searchLeaf :: Int -> SearchTree -> Bool, die fest-
stellt, ob der uebergebene Suchwert als Schluessel in einem Blatt der Baums vorkommt,
und eine Funktion searchStrings :: Int -> SearchTree -> [[Char]], welche die
Liste aller Strings ausgibt, die in einem Blatt stehen, dessen Schuessel mit dem Suchwert
uebereinstimmt. Beide Funktionen sollen das Prinzip der Binaersuche beruecksichtigen,
indem das unnoetige Durchsuchen von Teilbaeumen vermieden wird, in denen ein Vorkom-
men des Suchwerts ausgeschlossen ist.

> searchLeaf :: Int -> SearchTree -> Bool
> searchLeaf i (Blatt v _)
>   | (i == v) = True
>   | otherwise = False
> searchLeaf i (Knoten v s1 s2)
>   | (i == v) = searchLeaf i s1 || searchLeaf i s2
>   | (i < v) = searchLeaf i s1
>   | (i > v) = searchLeaf i s2

> searchStrings :: Int -> SearchTree -> [[Char]]
> searchStrings i (Blatt v s)
>   | (i == v) = [s]
>   | otherwise = []
> searchStrings i (Knoten v s1 s2)
>   | (i == v) = searchStrings i s1 ++ searchStrings i s2
>   | (i < v) = searchStrings i s1
>   | (i > v) = searchStrings i s2
