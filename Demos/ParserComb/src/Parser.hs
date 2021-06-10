{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parser where

import Control.Applicative (Alternative (..))
import Data.Char (isDigit)

