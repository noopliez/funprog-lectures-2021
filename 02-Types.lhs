Typen

> data Bool' = BFalse | BTrue
>   deriving Show


> not' :: Bool' -> Bool'
> not' BFalse = BTrue
> not' BTrue = BFalse

> and' :: Bool' -> Bool' -> Bool'
> and' BFalse _ = BFalse
> and' BTrue  y = y

Char: Unicode Zeichen
String: Liste von Char; [ Char ]
Int: Maschinen Integer
Float: single precision IEEE floating point
Double: double precision IEEE floating point

Listen

> boollist = [ False, True, False ]
> ohm = ['O', 'H', 'M' ]
> ohm' = "OHM"

[] leere Liste

[[]] eine Liste, die eine leere Liste enthaelt

head :: [a] -> a
tail :: [a] -> [a]

Tupel

() :: ()
leeres Tupel

(False, True) :: (Bool, Bool)

(False, 'a') :: (Bool, Char)

("Georg", 'S', True) :: (String, Char, Bool)
                        ([Char], Char, Bool)

> add' :: (Int, Int) -> Int
> add' (x, y) = x + y

Curried Functions

> plus :: Int -> (Int -> Int)
> plus x y = x + y


Partiell angewendete Funktionen

> plus3 :: Int -> Int
> plus3 = plus 3

> plus3r :: Int -> Int
> plus3r = (flip plus) 3


> plus3' :: Int -> Int
> plus3' = (3 +)

> plus3r' :: Int -> Int
> plus3r' = (+ 3)

Parametrische Polymorphie

length :: [a] -> Int

fst :: (a, b) -> a
snd :: (a, b) -> b

take :: Int -> [a] -> [a]
zip :: [a] -> [b] -> [(a, b)]


> id :: a -> a
> id x = x








