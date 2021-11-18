List Comprehensions

> import Data.Char

Aus der Mengenlehre kennen wir die Komprehension als eine
Moeglichkeit, eine Menge zu definieren.

Beispiele:

M = { x | x \in N , x gerade}
M' = { x^2 | x \in N }

In Haskell:

[ x^2 | x <- [1..3] ] = [1, 4, 9]


> quadrate :: Integer -> [Integer]
> quadrate n = [ x^2 | x <- [1 .. n] ]

> paare :: [a] -> [b] -> [ (a, b) ]
> paare xs ys = [ (x, y) | x <- xs, y <- ys ]

λ> paare [1,2] [3,4,5]
[(1,3),(1,4),(1,5),(2,3),(2,4),(2,5)]
λ> paare [3,4,5] [1,2]
[(3,1),(3,2),(4,1),(4,2),(5,1),(5,2)]

Alternative Definition der Laenge einer Liste:

> laenge' :: [a] -> Int
> laenge' xs = sum [ 1 | _ <- xs ]

Liste der Ersten aus einer Liste von Paaren

> firsts :: [ (a, b) ] -> [a]
> firsts ps = [ x | (x, _) <- ps ]

Listen von Listen zu einer Liste aller Elemente

> concat ::[[a]] -> [a]
> concat xss = [ x | xs <- xss, x <- xs ]


Filtern 

Alle geraden Zahlen: { x | x \in N , x mod 2 = 0}

λ> [x | x <- [1..7], x `mod` 2 == 0 ]
[2,4,6]

> gerade :: [Int] -> [Int]
> gerade xs = [ x | x <- xs , x `mod` 2 == 0 ]

> faktoren :: Integer -> [Integer]
> faktoren n = [ x  | x <- [1..n], n `mod` x == 0 ]

Primzahlentest

> prim :: Integer -> Bool
> prim n = faktoren n == [1,n]

> primzahlen :: Integer -> [Integer]
> primzahlen n = [ x | x <- [1 .. n], prim x ]

Perfekte Zahl: Summe der Faktoren ohne die Zahl selbst
ist gleich der Zahl.

> perfekt :: Integer -> [Integer]
> perfekt n = [ x | x <- [1 ..n ], sum (init (faktoren x)) == x ]

Suchen in einer Map:

> find :: Eq a => a -> [(a, b)] -> [b]
> find k t = [ v | (k', v) <- t, k' == k ]

λ> zip "ohm" [1..6]
[('o',1),('h',2),('m',3)]

Ist eine Liste sortiert?

Fuer alle benachbarten Elemente x, y gilt x <= y.

> benachbart :: [a] -> [(a, a)]
> benachbart xs = zip xs (tail xs)

> sortiert :: Ord a => [a] -> Bool
> sortiert xs = and [ x <= y | (x, y) <- benachbart xs]

> positionen :: Eq a => a -> [a] -> [Int]
> positionen x xs = [ i | (x', i) <- zip xs [0..], x' == x ]


String comprehensions

Wieviele Kleinbuchstaben enthaelt ein String?

> kleine :: String -> Int
> kleine xs = length [ x | x <- xs, isLower x ]

> count :: Char -> String -> Int
> count x ys = length [y | y <- ys, x == y ]

Cryptographie aus der Antike (Caeser Chiffre)

> buch2int :: Char -> Int
> buch2int c = ord c - ord 'a'

> int2buch :: Int -> Char
> int2buch n = chr (ord 'a' + n)

> shift :: Int -> Char -> Char
> shift n c | isLower c = int2buch ((buch2int c + n) `mod` 26)
>           | otherwise = c

> encrypt :: Int -> String -> String
> encrypt n xs = [ shift n x | x <- xs]

> caesar :: String -> String
> caesar = encrypt 3

Tabelle der Haeufigkeiten in der deutschen Sprache (nach
Wikipedia https://de.wikipedia.org/wiki/Buchstabenhäufigkeit,
gerundet):

> table :: [Float]
> table = [ 6.5, 1.9, 3.1, 5.1, 17.4, 1.7, 3.0, 4.8, 7.6,
>           0.3, 1.2, 3.4, 2.5, 9.8, 2.5, 0.8, 0.02, 7.0,
>           7.3, 6.2, 4.4, 0.7, 1.9, 0.03, 0.04, 1.1 ]


> percent :: Int -> Int -> Float
> percent n m = (fromIntegral n / fromIntegral m) * 100

> frequenzen :: String -> [Float]
> frequenzen xs = [ percent (count x xs) (kleine xs) | x <- ['a' .. 'z']]

Chi-Quadrat Test

> chisqr :: [Float] -> [Float] -> Float
> chisqr os es = sum [ (o - e)^2 / e | (o, e) <- zip os es]

> rotate :: Int -> [a] -> [a]
> rotate n xs = drop n xs ++ take n xs

> crack :: String -> String
> crack xs = encrypt (-offset) xs
>   where offset = head (positionen (minimum chitable) chitable)
>         chitable = [ chisqr (rotate n freqtable) table | n <- [0 .. 25] ]
>         freqtable = frequenzen xs



