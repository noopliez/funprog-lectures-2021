Typklassen

> x = 1 + 2

> y = (1.0 :: Float) + 2.0

Standard Typklassen

Eq - Gleichheit

(==) :: a -> a -> Bool
(/=) :: a -> a -> Bool

Ord - Totalordnung

(<) :: a -> a -> Bool

(<=), (>=), (>) :: a -> a -> Bool

  max :: a -> a -> a
  min :: a -> a -> a

Show - darstellbare Typen

show :: a -> String

Read - einlesbare Typen

"Umkehrung von Show"

read :: String -> a

read (show x) :: a soll dasselbe Element vom Typ a liefern

show (read x) soll einen aehnlichen String liefern

Fractional

(/) :: a -> a -> a
recip :: a -> a  Kehrwert

Fuktionen

Neue Funktionen aus bestehenden Funktionen zusammensetzen.

> even' :: Integral a => a -> Bool
> even' n = n `mod` 2 == 0

> splitAt' :: Int -> [a] -> ([a], [a])
> splitAt' n xs = (take n xs, drop n xs)

Bedingte Ausdruecke

> abs' :: Int -> Int
> abs' n = if n >= 0 then n else -n

> signum' :: Int -> Int
> signum' n = if n < 0 then -1 else if n == 0 then 0 else 1

Guarded Equations
             /
             | -1, wenn n < 0
signum(n) = <   0, wenn n = 0
             |  1, sonst
             \


> sonst :: Bool
> sonst = True

> signum'' :: Int -> Int
> signum'' n | n < 0  = -1
>            | n == 0 = 0
>            | sonst  = 1

Tupel Patterns

> erster :: (a, b) -> a
> erster (x, _) = x

> zweiter :: (a, b) -> b
> zweiter (_, y) = y

Listen Patterns

String der Laenge 3, der mit 'a' beginnt.

> adrei :: [Char] -> Bool
> adrei ['a', _, _] = True
> adrei _          = False

Exkurs Listennotation

[1, 2, 3]
= {Notation Liste}
1 : [2, 3]
=
1 : (2 : [3])
=
1 : (2 : (3 : []))

> startA :: [Char] -> Bool
> startA ('a': _) = True
> startA _        = False

Anonyme Funktionen (Lambda Expressions)

\x -> x + x

Funktion zum Verdoppeln

Alternative Definition fuer add

> add'' :: Int -> (Int -> Int)
> add'' = \x -> (\y -> x + y)

> const :: a -> b -> a
> const x _ = x

> const :: a -> (b -> a)
> const x = \_ -> x

> odds :: Int -> [Int]
> odds n = map f [0 .. n-1]
>   where f x = x * 2 + 1

> odds' :: Int -> [Int]
> odds' n = map (\x -> 2 * x + 1) [0 .. n-1]

Operator Sections

(#) = \x -> (\y -> x # y)

(x #) = \y -> x # y

(# y) = \x -> x # y

Beispiele

(+) Additionsfunktion
(1+) Nachfolgerfunktion
(1/) Kehrwertfunktion
(*2) Verdoppelung

