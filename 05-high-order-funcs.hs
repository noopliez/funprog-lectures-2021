module HighOrderFuncs where

import Data.Char ( ord, chr )

-- VL vom 5.11.2021

{- High Order Functions -}

type Bit = Int

-- Alternative Definition der bin2int
bin2int' :: [Bit] -> Int
bin2int' bits = sum [ bit * w | (bit, w) <- zip bits weights ]
    where weights = iterate (*2) 1

-- Eigene Definition der iterate - Funktion
myiterate :: (a -> a) -> a -> [a]
myiterate f x = x : myiterate f (f x)

-- Aus GdI: Hornerschema
bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2 *y) 0 

-- Die inverse Funktion
int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

-- Oktetts, fülle 8 Bit auf
make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

-- Encodierfunktion
encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

-- Decodierfunktion
chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String 
decode = map (chr . bin2int) . chop8

-- Übertragung über einen perfekten Kanal (störungsfrei)
transmit :: String -> String 
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id