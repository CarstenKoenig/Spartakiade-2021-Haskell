{-# LANGUAGE DeriveFunctor #-}
module Teletyper.Version5 where
  
import Control.Monad ((>=>), ap)

data TeletyperF r
  = Write String r
  | Read (String -> r)
  deriving Functor

runF :: TeletyperF (IO a) -> IO a
runF (Write text cont) = putStrLn text >> cont
runF (Read cont) = getLine >>= cont


data Free f a
  = Pure a
  | Free (f (Free f a))
  deriving Functor

instance Functor f => Applicative (Free f) where
  pure = Pure
  (<*>) = ap

instance Functor f => Monad (Free f) where
  (Pure x) >>= f = f x
  (Free m) >>= f = Free (fmap (>>= f) m)

runFree :: (Monad m, Functor f) => (f (m a) -> m a) -> Free f a -> m a
runFree _ (Pure a) = pure a
runFree runFunctor (Free f) = runFunctor (fmap (runFree runFunctor) f)

type Teletyper = Free TeletyperF

writeTR :: String -> Teletyper ()
writeTR txt = Free (Write txt (Pure ()))

readTR :: Teletyper String
readTR = Free (Read Pure)

sayHello :: Teletyper ()
sayHello = do
  writeTR "What is your name?"
  name <- readTR
  writeTR ("Hello " ++ name)

run :: Teletyper a -> IO a
run = runFree runF