---
title: 'Haskell Workshop - Monaden-Transformer'
author: Carsten König
date: Juni 2021
geometry: margin=2cm
output: pdf_document
papersize: a4
---

## Reader

wird gerne verwendet um Konfiguration/Umgebung zur Verfügung zu stellen

```haskell
newtype Reader r a = Reader { runReader :: r -> a }
  deriving (Functor, Applicative)


instance Monad (Reader r) where
  m >>= f =
    Reader $ \r -> runReader (f (runReader m r)) r


ask :: Reader r r
ask = Reader id


reader :: (r -> a) -> Reader r a
reader = Reader


local :: (r -> r) -> Reader r a -> Reader r a
local adj m = Reader $ \r -> runReader m (adj r)
```

## State

```haskell
newtype State s a = State { runState :: s -> (a,s) }
  deriving (Functor)


instance Applicative (State s) where
  pure a = State $ \s -> (a,s)
  sf <*> sx = sf >>= (\f -> f <$> sx)


instance Monad (State s) where
  m >>= f = State $ \s ->
    let (a, s') = runState m s
    in runState (f a) s'


get :: State s s
get = State $ \s -> (s,s)


put :: s -> State s ()
put s = State $ \_ -> ((), s)


modify :: (s -> s) -> State s ()
modify adj = State $ \ s -> ((), adj s)


gets :: (s -> a) -> State s a
gets g = State $ \ s -> (g s, s)


evalState :: State s a -> s -> a
evalState m = fst . runState m


execState :: State s a -> s -> s
execState m = snd . runState m
```

## Transformer

Monaden sind leider nicht wie z.B. Funktoren komposierbar.
Deshalb _stacken_ wir sie mit Transformatoren.

Der Monadentransformator erweitert sozusagen einen Monaden um die Fähigkeiten eines anderen.

## ExceptT

```haskell
newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }
  deriving (Functor)


instance Monad m => Applicative (ExceptT e m) where
  pure = ExceptT . return . Right
  mf <*> mx = mf >>= (\f -> f <$> mx)


instance Monad m => Monad (ExceptT e m) where
  (ExceptT m) >>= f =
    ExceptT $ m >>= (\case (Left err) -> return (Left err)
                           (Right  a) -> runExceptT (f a))


liftE :: Monad m => m a -> ExceptT e m a
liftE m = ExceptT $ Right <$> m


throwErrorT :: Monad m => e -> ExceptT e m a
throwErrorT err = ExceptT (return $ Left err)


catchErrorT :: Monad m => ExceptT e m a -> (e -> ExceptT e m a) -> ExceptT e m a
catchErrorT m handler = ExceptT $ do
  res <- runExceptT m
  case res of
    Left err -> runExceptT $ handler err
    Right  a -> return (Right a)
```

## Klassen

damit nicht überall `lift` benutzt werden muss, führt **MTL** noch Typ-Klassen ein.
Zum Beispiel gibt es einen `MonadError`:

### MonadReader

siehe [Control.Monad.Reader](https://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-Reader.html)

```haskell
class Monad m => MonadReader r m | m -> r where
  ask :: m r
  local :: (r -> r) -> m a -> m a
  reader :: (r -> a) -> m a

-- Synonym für reader
asks :: MonadReader r m => (r -> a) -> m a
```

### MonadState

siehe [Control.Monad.State](https://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-State-Lazy.html)

```haskell
class Monad m => MonadState s m | m -> s where
  get :: m s
  put :: s -> m ()
  state : (s -> (a, s)) -> m a

gets :: MonadState s m => (s -> a) -> m a

modify :: MonadState s m => (s -> s) -> m ()
-- strikte Variante im neuen Zustand
modify' ...
```

### MonadTrans

`lift` funktioniert mit allen Transformatoren

siehe [Control.Monad.Trans.Class](https://hackage.haskell.org/package/transformers-0.5.2.0/docs/Control-Monad-Trans-Class.html)

```haskell
class MonadTrans t where
  lift :: Monad m => m a -> t m a
```

### MonadIO

`liftIO` funktioniert mit allen Transformatoren

siehe [Control.Monad.IO.Class](https://hackage.haskell.org/package/base-4.9.1.0/docs/Control-Monad-IO-Class.html#t:MonadIO)

```haskell
class Monad m => MonadIO m where
  liftIO :: IO a -> m a
```

## interessante Links

- Mark P. Jones: [Functional Programming with Overloading and Higher-Order Polymorphism](http://web.cecs.pdx.edu/~mpj/pubs/springschool.html)
