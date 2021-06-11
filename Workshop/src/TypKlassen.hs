{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}

module TypKlassen where

-- show :: Show a => a -> String

-- Ein paar Details
-- https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/instances.html

-- analog Show
class MyShow a where
  myShow :: a -> String

-- Show / Read
-- Num
-- Eq
-- Ord

-----

-- Functor (Scale: Mapable)
-- type Functor :: (* -> *) -> Constraint  <- Kind-Signatur (kann man ignorieren)
-- class Functor f where
--    fmap :: (a -> b) -> (f a -> f b)

-- Identity
--   fmap id == id
-- Composition
--   fmap (f . g) == fmap f . fmap g

-- fmap = <$>
-- fmap (+1) [1..4] = [2..5]
--       ^ Section (+1) = (\i -> i + 1)

data Pair a
  = MkPair a a
  deriving Show

instance Functor Pair where
  -- :: (a -> b) -> Pair a -> Pair b
  fmap f (MkPair a1 a2) = MkPair (f a1) (f a2)

data ITree a
  = Leaf (Int -> a)
  | Node [ITree a]
  deriving Functor

{-
instance Functor ITree where
  fmap f (Leaf fi) = Leaf (f . fi)
  fmap f (Node subtrees) = Node (fmap (fmap f) subtrees)
-}

-- Wo geht Functor
-- in postivier Position: wenn es so dasteht oder rechts vom ->
-- nach links vom -> kehrt sich positiv/negativ um
-- a -> Int (geht nicht)
-- ((a -> b) -> b) geht ...

--- Sind Funktoren "verkettbar"

newtype Compose f g a = MkCompose (f (g a))

-- Beispiel: f = [], g = Maybe
instance (Functor f, Functor g) => Functor (Compose f g) where
-- Beispiel x = [Just 5]
  fmap f (MkCompose x) = MkCompose (fmap (fmap f) x)
-- (oder `(fmap . fmap)`)

-- Beispiel: f = [], g = Maybe => entweder Left [1,2] oder Right (Just 42)
newtype Coproduct f g a = MkCoproduct (Either (f a) (g a))

instance (Functor f, Functor g) => Functor (Coproduct f g) where
  fmap f (MkCoproduct (Left x)) = MkCoproduct (Left (fmap f x))
  fmap f (MkCoproduct (Right y)) = MkCoproduct (Right (fmap f y))

---

-- Exotische Funktoren

newtype Ident a = MkIdent a

instance Functor Ident where
  fmap f (MkIdent a) = MkIdent (f a)

newtype Const b a = MkConst b

instance Functor (Const b) where
  fmap _ (MkConst b) = MkConst b


-- Recursion Schemes (Rekursive Datentypen - generisch behandeln) (Scrap your Boilerplate ..)


-- Mittagspause bis 13:30 - bin ab 13:15 zurück falls Fagen sind