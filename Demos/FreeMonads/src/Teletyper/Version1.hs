module Teletyper.Version1 where

data Teletyper
  = Write String Teletyper
  | Read (String -> Teletyper)
  | Done

sayHello :: Teletyper
sayHello =
  Write "What is your name?" (Read (\name -> Write ("Hello " ++ name) Done))

run :: Teletyper -> IO ()
run Done =
  pure ()
run (Write text cont) = do
  putStrLn text
  run cont
run (Read cont) = do
  s <- getLine
  run (cont s)
