{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Teletyper.VersionFused where

-- siehe: https://github.com/fused-effects/fused-effects/blob/master/docs/defining_effects.md

import Control.Algebra (Algebra, Has, alg, send, (:+:)(..))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Kind

-- Syntax / Effect
data Teletype (m :: Type -> Type) k where
  Read  ::           Teletype m String
  Write :: String -> Teletype m ()  

readTR :: Has Teletype sig m => m String
readTR = send Read

writeTR :: Has Teletype sig m => String -> m ()
writeTR s = send (Write s)

-- Carrier (Semantic) Effect
newtype TeletypeIOC m a = TeletypeIOC { runTeletypeIO :: m a }
  deriving (Applicative, Functor, Monad, MonadIO)

-- Algebra-Instanz für den Carrier (Interpretation)
instance (MonadIO m, Algebra sig m) => Algebra (Teletype :+: sig) (TeletypeIOC m) where
  alg hdl sig ctx = case sig of
    L Read      -> (<$ ctx) <$> liftIO getLine
    L (Write s) -> ctx      <$  liftIO (putStrLn s)
    R other     -> TeletypeIOC (alg (runTeletypeIO . hdl) other ctx)

-- Sag Hallo

sayHello :: Has Teletype sig m => m ()
sayHello = do
  writeTR "What is your name?"
  name <- readTR
  writeTR ("Hello " ++ name)

run :: TeletypeIOC m a -> m a
run = runTeletypeIO