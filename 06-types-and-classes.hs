{-# LANGUAGE InstanceSigs #-}
module TypesAndClasses where
    -- VL vom 12.11.2021

    {- Typen und Typklassen -}

    -- Typsynonyme 
    type MyString = [Char]

    -- Koordinaten in 2D
    type Position = (Int, Int)

    -- Koordinaten in 3D
    type Koordinaten = (Float, Float, Float)

    -- Transformation von 3D Koordinaten 
    type Transformation = Koordinaten -> Koordinaten

    -- Rekursion
    -- type Tree = (Int, [Tree]) => Funktioniert nicht

    -- Parameter sind erlaubt
    -- Bsp: Homogenes Paar
    type HomogenesPaar a = (a, a)

    -- Auch mehrere Parameter
    type Tabelle key value = [(key, value)]

    find :: Eq k => k -> Tabelle k v -> v
    find s t = head [v | (s', v) <- t, s' == s]

    type Schluessel = Int
    type IntTabelle v = Tabelle Schluessel v

    -- Neue Datentypen
    data Unit = Unit -- in Haskell: () :: ()

    data MyBool = Falsch | Wahr

    toMyBool :: Bool -> MyBool
    toMyBool False = Falsch
    toMyBool True = Wahr

    -- Himmelsrichtungen
    data Richtung = Nord | Sued | Ost | West

    -- Bewegung im Koordinatensystem 
    schritt :: Richtung -> Position -> Position
    schritt Nord (x, y) = (x, y + 1)
    schritt Sued (x, y) = (x, y - 1)
    schritt Ost (x, y) = (x + 1, y)
    schritt West (x, y) = (x - 1, y)

    schritte :: [Richtung] -> Position -> Position
    schritte []     p = p 
    schritte (x:xs) p = schritte xs (schritt x p)

    -- High-Order Function
    schritte' :: [Richtung] -> Position -> Position
    schritte' xs p = foldl (flip schritt) p xs

    -- Das berühmte Formenbeispiel
    data Form = Kreis Float | Rechteck Float Float

    quadrat :: Float -> Form
    quadrat s = Rechteck s s

    flaeche :: Form -> Float
    flaeche (Kreis r) = r^2 * pi 
    flaeche (Rechteck l b) = l * b 

    -- Typen mit Parametern
    data Option a = None | Some a

    safediv :: Int -> Int -> Option Int
    safediv _ 0 = None
    safediv m n = Some (m `div` n)

    safehead :: [a] -> Option a
    safehead []    = None
    safehead (x:_) = Some x -- safehead xs = Some (head xs)

    -- Newtype
    newtype Nat' = N' Int

    -- Unterscheide
    type Nat'' = Int -- Nat' ist KEIN Synonym für Int

    -- Unterscheide weiter
    data Nat''' = N''' Int

    data Nat = Z | S Nat deriving Show

    -- Konversionsfunktionen
    nat2int :: Nat -> Int
    nat2int Z      = 0
    nat2int (S n') = 1 + nat2int n'

    int2nat :: Int -> Nat
    int2nat 0 = Z
    int2nat n = S (int2nat (n -1))

    add' :: Nat -> Nat -> Nat
    add' m n = int2nat (nat2int m + nat2int n)

    -- Rekursiv direkt
    add'' :: Nat -> Nat -> Nat
    add'' Z n    = n
    add'' (S m') n = S (add'' m' n)

    -- Polymorphe Liste
    data Liste a = Nil | Cons a (Liste a)

    laenge :: Liste a -> Int
    laenge Nil         = 0 
    laenge (Cons _ xs) = 1 + laenge xs

    -- Binäre Bäume
    data Baum a = Blatt a | Knoten (Baum a) a (Baum a)

    t :: Baum Int
    t = Knoten (Knoten (Blatt 2)  5 (Blatt  7)) 8 
            (Knoten (Blatt 9) 12 (Blatt 14))

    enthaelt :: Eq a => a -> Baum a -> Bool 
    enthaelt x (Blatt y)      = x == y
    enthaelt x (Knoten l y r) = x == y || enthaelt x l || enthaelt x r

    flatten :: Baum a -> [a]
    flatten (Blatt x)      = [x]
    flatten (Knoten l x r) = flatten l ++ [x] ++ flatten r

    -- Suchbaum 
    enthaelt' :: Ord a => a -> Baum a -> Bool
    enthaelt' x (Blatt y)      = x == y 
    enthaelt' x (Knoten l y r) | x == y    = True
                            | x < y     = enthaelt' x l
                            | otherwise = enthaelt' x r

    data RBaum a = RNode a [RBaum a]

    -- Typklassen
    class MEq a where 
        meq, mne :: a -> a -> Bool 
        x `mne` y = not (x `meq` y)
        x `meq` y = not (x `mne` y)

    -- Bsp einer Instanz für Bool
    instance MEq Bool where
        meq :: Bool -> Bool -> Bool 
        False `meq` False = True 
        True  `meq` True  = True 
        _     `meq` _     = False 

    {- Vererbung -}
    -- Bsp die Ord Klasse
    class MEq a => MOrd a where
        lt, leq, gt, geq :: a -> a -> Bool 
        min, max         :: a -> a -> a
        min x y | x `leq` y = x
                | otherwise = y
        max x y | x `leq` y = y
                | otherwise = x

    -- Instanz für Bool
    instance MOrd Bool where 
        False `lt` True = True 
        _     `lt` _    = False

        b `leq` c = (b `leq` c) || (b `meq` c)
        b `gt` c = c `lt` b
        b `geq` c = c `leq` b 

    data MyBool' = Falsch' | Wahr'
        deriving (Eq, Ord, Show, Read)

    {- Abstrakte Maschine -}
    -- Hutton's Razor
    data Expr = Val Int | Add Expr Expr 
    
    -- Bsp: (5 + 21) + 16
    bsp :: Expr
    bsp = Add (Add (Val 5) (Val 21)) (Val 16)

    -- Definiere Semantik
    value :: Expr -> Int
    value (Val n)   = n
    value (Add x y) = value x + value y

    type Cont = [Op]

    data Op = EVAL Expr | ADD Int 

    eval :: Expr -> Cont -> Int
    eval (Val n) c   = exec c n 
    eval (Add x y) c = eval x (EVAL y : c)

    exec :: Cont -> Int -> Int
    exec [] n = n
    exec (EVAL y : c) n = eval y (ADD n : c)
    exec (ADD n : c) m = exec c (n + m)

    value' :: Expr -> Int 
    value' e = eval e []
