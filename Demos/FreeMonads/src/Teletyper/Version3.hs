{-# LANGUAGE DeriveFunctor #-}
module Teletyper.Version3 where
  
import Control.Monad ((>=>), ap)

data TeletyperF r
  = Write String r
  | Read (String -> r)
  deriving Functor

data Teletyper a
  = Done a
  | Rec (TeletyperF (Teletyper a))
  deriving Functor

instance Applicative Teletyper where
  pure = Done
  (<*>) = ap

instance Monad Teletyper where
  (Done x) >>= f = f x
  (Rec ttf) >>= f = Rec (fmap (>>= f) ttf)

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
run (Done a) =
  pure a
run (Rec (Write text cont)) = do
  putStrLn text
  run cont
run (Rec (Read cont)) = do
  s <- getLine
  run (cont s)