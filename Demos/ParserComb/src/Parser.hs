{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parser
  ( Parser
  , parse
  , runParser
  , succeed
  , Parser.fail
  , one
  , digit
  , char
  , try
  , oneOf
  , many
  , many1
  , chainl
  , chainl1
  , between
  , whitespace
  ) where

import Control.Applicative (Alternative (..))
import Data.Char (isDigit)


parse :: Parser a -> String -> Maybe a
parse pa = fmap fst . runParser pa


newtype Parser a
  = Parser
  { runParser :: String -> Maybe (a, String)
  }


succeed :: a -> Parser a
succeed x = Parser $ \s -> Just (x, s)


fail :: Parser a
fail = Parser $ const Nothing


one :: Parser Char
one = char (const True)


digit :: Parser Char
digit = char isDigit


char :: (Char -> Bool) -> Parser Char
char praed = Parser $ \case
  (c:s) | praed c -> Just (c, s)
  _               -> Nothing


instance Functor Parser where
  fmap f p =
    Parser $ fmap (\(x, rest) -> (f x, rest)) . runParser p


try :: Parser a -> Parser (Maybe a)
try pa = Parser $ \s ->
  case runParser pa s of
    Nothing      -> Just (Nothing, s)
    Just (a, s') -> Just (Just a, s')


instance Applicative Parser where
  pure = succeed
  pf <*> pa = Parser $ \s -> do
    (f, s') <- runParser pf s
    (x, s'') <- runParser pa s'
    return (f x, s'')


instance Alternative Parser where
  empty     = Parser.fail
  p1 <|> p2 = Parser $ \s ->
    case runParser p1 s of
      ok@(Just _) -> ok
      Nothing     -> runParser p2 s


many1 :: Parser a -> Parser [a]
many1 = some


oneOf :: [Parser a] -> Parser a
oneOf = foldr (<|>) empty


instance Semigroup a => Semigroup (Parser a) where
  p1 <> p2 = (<>) <$> p1 <*> p2

instance Monoid a => Monoid (Parser a) where
  mempty = succeed mempty


instance Monad Parser where
  return     = pure
  pa >>= fpb = Parser $ \s -> do
    (a, s') <- runParser pa s
    runParser (fpb a) s'


between :: Parser l -> Parser r -> Parser a -> Parser a
between pl pr pa = pl *> pa <* pr


whitespace :: Parser ()
whitespace = return () <* many (char (`elem` " \n\t\r"))


chainl1 :: forall a . Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 pa pop = pa >>= more
  where
    more a =
      do
        op <- pop
        a' <- pa
        more (a `op` a')
      <|> succeed a


chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl pa pop va = chainl1 pa pop <|> succeed va