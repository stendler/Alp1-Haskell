Uebung 12 -- Alp1: Funktionale Programmierung
Tutor : Ha Do
Bearbeiter: Jasmine Cavael, Canel Frenschock, Maximilian Stendler

Goldbachsche Vermutung (2 + 3 + 4 Punkte)
Die Goldbachsche Vermutung besagt, dass jede gerade Zahl n ≥ 4 als Summe von zwei
Primzahlen dargestellt werden kann (1 ist keine Primzahl!).

a) Definieren Sie einen Primzahltest prim::Integer->Bool und beschreiben Sie dann
mit List-Comprehension eine Funktion goldbachSumme::Integer->[(Integer,Integer)],
die fuer jeden geraden Eingabewert alle Zerlegungen dieser Zahl in die Summe aus zwei
Primzahlen berechnet, wobei jedes Paar (a,b) aus der Liste die Ungleichung a ≤ b
eruellen soll. Was bei ungeraden Eingabewerten passiert ist egal.

> prim :: Integer -> Bool
> prim n
>  | (n<=1) = False
>  | (n==2) = True
>  | otherwise = help 2 n
>  where
>    help :: Integer -> Integer -> Bool
>    help pos n
>      | (mod n pos == 0) = False
>      | ((pos >= (sqrtI n)) && ((mod n pos) /=0)) = True
>      | otherwise = help (pos+1) n
> -- wurzel ziehen
> intSqrt :: Integer -> Integer -> Integer
> intSqrt n a
>   | (a*a == n) = a
>   | (a*a >= n) = a-1
>   | otherwise = intSqrt n (a+1)

> sqrtI :: Integer -> Integer
> sqrtI n = intSqrt n 0

> goldbachSumme :: Integer -> [(Integer,Integer)]
> goldbachSumme i = [(x,y)|x<-[0..i],y<-[0..i],x<=y,x+y==i,prim x,prim y]

b) Definieren Sie in einer Zeile (d.h. ohne Rekursion und nur mit prim als Hilfsfunktion)
die folgenden Listen und Funktionen:

> goldbachListe :: [(Integer,Integer)]
> goldbachListe = [head[(x,y)|x<-[2,3..z],y<-[2,3..z],x<=y,x+y==z,prim x,prim y]|z<-[4,6..]]

> goldbachMax :: Integer -> Integer
> goldbachMax n = maximum[(toInteger l)|z<-[4,6..n],l<-[length[(x,y)|x<-[2,3..z],y<-[2,3..z],x<=y,x+y==z,prim x,prim y]]]

> minForManyPairs :: Int -> Integer
> minForManyPairs k = head[z|z<-[4,6..],l<-[length[(x,y)|x<-[2,3..z],y<-[2,3..z],x<=y,x+y==z,prim x,prim y]],l==k]

Die erste Funktion soll eine unendliche Liste von Primzahlpaaren erzeugen deren Summe
die Zahlen 4, 6, 8, . . . ergibt (nur ein Paar pro Zahl).
Die zweite Funktion soll bei Eingabe
n die maximale Anzahl k von Primzahlpaaren (a 1 , b 1 ), (a 2 , b 2 ), . . . (a k , b k ) angeben, so
dass a 1 ≤ b 1 , a 2 ≤ b 2 , . . . , a k ≤ b k und a 1 + b 1 = a 2 + b 2 = . . . = a k + b k = m ≤ n.
Die dritte Funktion soll bei Eingabewert k die kleinste gerade Zahl n bestimmen, welche
mindestens k geordnete Goldbachpaare besitzt.
