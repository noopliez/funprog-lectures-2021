

> quadrat :: Integer -> Integer
> quadrat x = x * x

quadrat (7 + 8)
= { (+) auswerten }
quadrat 15
= {quadrat anwenden}
15 * 15
= { (*) auswerten }
225

Alternative:

quadrat (7 + 8)
= { quadrat anwenden }
(7 + 8) * (7 + 8)
= { (+) auswerten
15 * 15
= { (*) auswerten }
225

Imperatives Programmieren:

int akku = 1;
int n = 6;
for (int count = 1;  count <= n ; count ++)
  akku *= count;

In Haskell:
product [1 .. n]

Beispiel:

product [1 .. 6]
= { Range expandieren }
product [1,2,3,4,5,6]
= {product auswerten }
720

> produkt :: [ Integer ] -> Integer
> produkt [] = 1
> produkt (x:xs) = x * produkt xs

> ntes :: [Int] -> Int -> Int
> ntes [] n = error "Index out of bounds"
> ntes (x:xs) 0 = x
> ntes (x:xs) n = ntes xs (n -1)

> hoch4 :: Integer -> Integer
> hoch4 x = quadrat (quadrat x)

> fakultaet :: Integer -> Integer
> fakultaet n  = produkt [1 .. n]

> fakultaet' :: Integer -> Integer
> fakultaet' 0 = 1
> fakultaet' n = n * fakultaet' (n -1)

> quick :: Ord a => [a] -> [a]
> quick [] = []
> quick (x:xs) = quick kleiner ++ [x] ++ quick groesser
>    where
>       kleiner = [ k | k <- xs , k <= x]
>       groesser = [ g | g <- xs , g > x ]

