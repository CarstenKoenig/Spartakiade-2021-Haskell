{-# LANGUAGE DeriveFunctor #-}
module Teletyper.Version2 where
  
import Control.Monad ((>=>), ap)

data Teletyper a
  = Write String (Teletyper a)
  | Read (String -> Teletyper a)
  | Done a
  deriving Functor

instance Applicative Teletyper where
  pure = Done
  (<*>) = ap

instance Monad Teletyper where
  (Done x) >>= f = f x
  (Write txt cont) >>= f = Write txt (cont >>= f)
  (Read cont) >>= f = Read (cont >=> f)

writeTR :: String -> Teletyper ()
writeTR txt = Write txt (Done ())

readTR :: Teletyper String
readTR = Read Done

sayHello :: Teletyper ()
sayHello = do
  writeTR "What is your name?"
  name <- readTR
  writeTR ("Hello " ++ name)

run :: Teletyper a -> IO a
run (Done a) =
  pure a
run (Write text cont) = do
  putStrLn text
  run cont
run (Read cont) = do
  s <- getLine
  run (cont s)