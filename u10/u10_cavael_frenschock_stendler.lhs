Uebung 10 -- Alp1: Funktionale Programmierung
Tutor : Ha Do
Bearbeiter: Jasmine Cavael, Canel Frenschock, Maximilian Stendler

Aufgabe 2: Baumtraversierungen
(2 + 4 + 3 Punkte)
a) Definieren Sie algenbraische Typen BinTree1, BinTree2 und BinTree3 fuer echte
binaere Baeume, wobei fuer BinTree1 nur die inneren Knoten, fuer BinTree2 nur die Blaetter
und fuer BinTree3 alle Knoten mit Int–Werten markiert werden sollen.

> data BinTree1 = Blatt1 | Knoten1 Int BinTree1 BinTree1
> data BinTree2 = Blatt2 Int | Knoten2 BinTree2 BinTree2
> data BinTree3 = Blatt3 Int | Knoten3 Int BinTree3 BinTree3

b) Implementieren Sie die folgenden Funktionen:
countOddLabels1 :: BinTree1 -> Int soll die Anzahl der Knoten mit ungerader
Markierung zaehlen

> countOddLabels1 :: BinTree1 -> Int
> countOddLabels1 (Blatt1) = 0
> countOddLabels1 (Knoten1 i bt1a bt1b)
>    | ((mod i 2) == 1) = 1 + (countOddLabels1 bt1a) + (countOddLabels1 bt1b)
>    | otherwise = (countOddLabels1 bt1a) + (countOddLabels1 bt1b)

spanOfLabels2 :: BinTree2 -> Int soll die Differenz zwischen groeßter und kleinster
Markierung ausgeben.

spanOfLabels2 (Knoten (Blatt i1) (Blatt i2)) = max(i1,i2)

> spanOfLabels2 :: BinTree2 -> Int
> spanOfLabels2 (Blatt2 i) = 0
> spanOfLabels2 root = (maxLabels2 root) - (minLabels2 root)
>   where
>     maxLabels2 (Knoten2 (Blatt2 i1) (Blatt2 i2)) = max i1 i2
>     maxLabels2 (Knoten2 bt2a bt2b) = max (maxLabels2 bt2a) (maxLabels2 bt2b)
>     minLabels2 (Knoten2 (Blatt2 i1) (Blatt2 i2)) = min i1 i2
>     minLabels2 (Knoten2 bt2a bt2b) = min (minLabels2 bt2a) (minLabels2 bt2b)

countEvenPaths3 :: BinTree3 -> Int soll die Anzahl der Wege von der Wurzel bis
zu einem Blatt ausgeben, auf denen die Summe aller Markierungen (einschl. Wurzel
und Blatt) gerade ist.

> countEvenPaths3 :: BinTree3 -> Int
> countEvenPaths3 bt3 = count bt3 0
>   where
>     count :: BinTree3 -> Int -> Int
>     count (Blatt3 i) s
>       | ((mod (i+s) 2) == 0) = 1
>       | otherwise = 0
>     count (Knoten3 i bt3a bt3b) s = (count bt3a (i+s)) + (count bt3b (i+s))

c) Implementieren Sie Funktionen traverse1, traverse2 bzw. traverse3 auf BinTree1,
BinTree2 bzw. BinTree3, welche die Liste der Knotenmarkierungen bei 1) Inorder–, 2)
Preorder– bzw. 3) Postorder–Traversierungen der Baeume ausgeben.

> traverse1 :: BinTree1 -> [Int]
> traverse1 (Blatt1) = []
> traverse1 (Knoten1 i (Blatt1) (Blatt1)) = [i]
> traverse1 (Knoten1 i bt1 bt2) = (traverse1 bt1)++[i]++(traverse1 bt2)

> traverse2 :: BinTree2 -> [Int]
> traverse2 (Blatt2 i) = [i]
> traverse2 (Knoten2 bt1 bt2) = (traverse2 bt1)++(traverse2 bt2)

> traverse3 :: BinTree3 -> [Int]
> traverse3 (Blatt3 i) = [i]
> traverse3 (Knoten3 i bt1 bt2) = (traverse3 bt1)++(traverse3 bt2)++[i]
