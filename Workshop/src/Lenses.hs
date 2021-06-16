{-# LANGUAGE RankNTypes #-}
module Lenses where

import Data.Functor.Identity
import Control.Applicative

data Name = Name
  { vorname :: String
  , nachname :: String
  }
  deriving Show

data Person = Person
  { name :: Name
  , alter :: Int
  }
  deriving Show

carsten :: Person
carsten = Person (Name "Carsten" "König") 41

carsten' = carsten { name = (name carsten) { vorname = "dkdfak" } }

data Lens1 s a = Lens
  { view1 :: s -> a
  , set1 :: a -> s -> s
  }

vornameLens1 :: Lens1 Name String
vornameLens1 = Lens vorname (\vn n -> n { vorname = vn })

nameLens1 :: Lens1 Person Name
nameLens1 = Lens name (\n p -> p { name = n})

setzeVorname :: String -> Person -> Person
setzeVorname vn p =
  let alterName = view1 nameLens1 p
      neuerName = set1 vornameLens1 vn alterName
  in set1 nameLens1 neuerName p

data Lens2 s a = Lens2
  { view2 :: s -> a
  , over2 :: (a -> a) -> s -> s
  }

set2 :: Lens2 s a -> a -> s -> s
set2 l a s = over2 l (const a) s


vornameLens2 :: Lens2 Name String
vornameLens2 = Lens2 vorname (\f n -> n { vorname = f (vorname n) })

nameLens2 :: Lens2 Person Name
nameLens2 = Lens2 name (\f p -> p { name = f (name p)})

setzeVorname2 :: String -> Person -> Person
setzeVorname2 vn =
  over2 nameLens2 (set2 vornameLens2 vn) 

data Lens3 s a = Lens3
  { view3 :: s -> a
  , over3 :: (a -> a) -> s -> s
  , overIO3 :: (a -> IO a) -> s -> IO s
  }

-- van Laarhoven Lens
type VLLens s a =
  forall f. Functor f => (a -> f a) -> s -> f s

overVL :: VLLens s a -> (a -> a) -> s -> s
overVL l f s = runIdentity $ l (Identity . f) s

viewVL :: VLLens s a -> s -> a
viewVL l s = getConst $ l Const s

vornameLensVL :: VLLens Name String
vornameLensVL f n = 
  fmap (\v -> n { vorname = v }) (f (vorname n))

nameLensVL :: VLLens Person Name
nameLensVL f p = 
  fmap (\n -> p { name = n }) (f (name p))

personVorname :: VLLens Person String
personVorname = nameLensVL . vornameLensVL