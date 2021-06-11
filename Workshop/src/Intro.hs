{-# LANGUAGE RankNTypes #-}
module Intro where

import Data.Function ((&))

{-
Console:

(neues Projekt)
stack new Workshop new-template

cd Workshop

stack setup
stack build

---

Main.hs

-- main = geändert
main = putStrLn  "Hallo Spartakiade"

---

stack run

...

Hallo Spartakiade
-}

-- Haskell spezifisch
-- pure (IO)
-- lazy
-- Typ Klassen (!)
-- Typ-Ebene ... 

z :: Int
z = 5

-- String ~ [Char]
-- (Alternative :: Text / ByteString)
text :: String
text = "String"

-- Unit
unitWert :: ()
unitWert = ()

-- Bottom

bottom = undefined

endlosSchleife = endlosSchleife

-- Lazy
liste = [1, undefined]

funktioniert = take 1 liste -- = 1

-- Strict / Nicht-Strict
-- Funktion f heißt wenn f undefined = undefined

-- Beispiel &&

-- Funktionen

-- Funktionale Sprachen
plus :: Int -> (Int -> Int)
plus x y = x + y

-- partial application
plus5 :: Int -> Int
plus5 = plus 5

-- "herkömlichen" Sprachen
plus' :: (Int, Int) -> Int
plus' (x,y) = x + y

-- Komposition von Funktionen
toString :: Int -> String
toString = show

-- f x = toString (plus5 x)
-- (.) = Funktions-Komposition
f :: Int -> String
f = toString . plus5

-- Pipe-Operator (F# |>)
-- Haskell Data.Function ((&))
f' :: Int -> String
f' x = x & plus5 & toString

-- ($) - Operator ("implizite Klammer bis zum Ende der Zeile")
f'' :: Int -> String
-- anstatt toString (plus5 x)
f'' x = toString $ plus5 x

---

-- Tupel
-- (x,y,z)

-- Listen
-- [1,2,3]
-- Ranges: [1..5], [1,3..5]
-- Listen sind rekursiv:
-- [1,2,3] = 1:(2:(3:[]))

len :: [a] -> Int
len [] = 0
len (_:xs) = 1 + len xs

-- List-Comprehensions
geradeQuadratzahlen :: [Integer]
geradeQuadratzahlen = [ zahl^(2 :: Int) | zahl <- [(1::Integer)..], even zahl ]

-- Haskell: Maybe
data Option a
  = None
  | Some a
  deriving (Show)

withDefault :: a -> Option a -> a
withDefault d None = d
withDefault _ (Some a) = a

-- IO / Monaden / do

ioAction :: IO ()
ioAction = putStrLn "Hallo"

-- (>>=) :: IO a -> (a -> IO b) -> IO b

ioHallo :: IO ()
ioHallo = getLine >>= (\s -> putStrLn ("Hallo " ++ s))

ioHallo' :: IO ()
ioHallo' = do
  s <- getLine
  putStrLn ("Hallo " ++ s)

ioHallo''' :: IO ()
ioHallo''' = getLine >> putStrLn "Hallo"

allePaare :: [(Int,Char)]
allePaare = do
  x <- [1..5]
  y <- ['a'..'c']
  -- oder `return`
  pure (x,y)

-- Was sind Higher-Kinded-Types

-- 5 <- Wert
-- :: Int <- Typ
-- :: * <- Kind (gibt * (*->*)) - Type

-- Maybe Int <- Kind *
-- Maybe <- Kind? ... * -> *

-------

-- Was sind Higher-Ranks

showStuff :: (forall a. Show a => a -> String) -> String
showStuff s = s 5 ++ s False -- = showStuff show -> "5False"