

Alp1 - Uebung 11

> import Data.String

Aufgabe 1: Binaersuche

Implementieren den Datentyp SearchTree als echten Binaerbaum in dem jeder Knoten
einen Int-Wert als Schluessel hat und jedes Blatt zusaetzlich einen String beinhaltet.

> data SearchTree = Blatt Int String | Knoten Int SearchTree SearchTree

a) Implementieren Sie eine Funktion checkSTProp :: SearchTree -> Bool, die ueber-
prueft, ob der Baum mit den gegebenen Schluesseln ein binaerer Suchbaum ist, und eine
Funktion search :: Int -> BinTree1 -> Bool, die (unter Voraussetzung der Such-
baumeigenschaft) mit Binaersuche feststellt, ob der uebergebene Suchwert als Schluessel
im Baum vorkommt.

> checkSTProp :: SearchTree -> Bool
> checkSTProp

b) Ab jetzt setzen wir voraus, dass alle als SearchTree verwendeten Objekte die Such-
baumeigenschaft haben.
Implementieren Sie eine Funktion searchLeaf :: Int -> SearchTree2 -> Bool, die fest-
stellt, ob der uebergebene Suchwert als Schluessel in einem Blatt der Baums vorkommt,
und eine Funktion searchStrings :: Int -> SearchTree2 -> [[Char]], welche die
Liste aller Strings ausgibt, die in einem Blatt stehen, dessen Schuessel mit dem Suchwert
uebereinstimmt. Beide Funktionen sollen das Prinzip der Binaersuche beruecksichtigen,
indem das unnoetige Durchsuchen von Teilbaeumen vermieden wird, in denen ein Vorkom-
men des Suchwerts ausgeschlossen ist.
