{-# LANGUAGE DeriveFunctor #-}
module Teletyper.Version4 where
  
import Control.Monad ((>=>), ap)

data TeletyperF r
  = Write String r
  | Read (String -> r)
  deriving Functor

runF :: TeletyperF (IO a) -> IO a
runF (Write text cont) = putStrLn text >> cont
runF (Read cont) = getLine >>= cont

data Teletyper a
  = Done a
  | Rec (TeletyperF (Teletyper a))
  deriving Functor


instance Applicative Teletyper where
  pure = Done
  (<*>) = ap

instance Monad Teletyper where
  (Done x) >>= f = f x
  (Rec (Write txt cont)) >>= f = Rec (Write txt (cont >>= f))
  (Rec (Read cont)) >>= f = Rec (Read (\s -> cont s >>= f))

writeTR :: String -> Teletyper ()
writeTR txt = Rec (Write txt (Done ()))

readTR :: Teletyper String
readTR = Rec (Read Done)

sayHello :: Teletyper ()
sayHello = do
  writeTR "What is your name?"
  name <- readTR
  writeTR ("Hello " ++ name)

run :: Teletyper a -> IO a
run (Done a) = pure a
run (Rec tf) = runF (fmap run tf)