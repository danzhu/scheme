{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Parser (
  Parser,
  Stream,

  parse,

  advance,
  peek,
  eof,
  satisfy,
  expect,
  expects,
  oneOf,
  noneOf,
  between,

  try,
  many1,
  sepBy,
  sepBy1,
  endBy,
  leftRec,
  ) where

import Control.Applicative (Alternative, (<|>), empty, many, optional, liftA2)
import Control.Arrow (first)
import Control.Monad (MonadPlus, mfilter, void)
import qualified Data.List as List


-- Parser type
newtype Parser s a = Parser { parse :: s -> Maybe (a, s) }

instance Functor (Parser s) where
  fmap f p = Parser par
    where par s = first f <$> parse p s

instance Applicative (Parser s) where
  p <*> q = Parser par
    where par s = do (f, s') <- parse p s
                     first f <$> parse q s'

  pure x = Parser par
    where par s = pure (x, s)

instance Monad (Parser s) where
  p >>= f = Parser par
    where par s = do (a, s') <- parse p s
                     parse (f a) s'

  fail _ = empty

instance Alternative (Parser s) where
  p <|> q = Parser par
    where par s = parse p s <|> parse q s

  empty = Parser $ const empty

instance MonadPlus (Parser s)

instance Monoid (Parser s a) where
  mappend = (<|>)

  mempty = empty


class Stream s t | s -> t where
  stream :: s -> Maybe (t, s)

instance Stream [t] t where
  stream = List.uncons


-- primitives
advance :: (Stream s t) => Parser s t
advance = Parser stream

peek :: (Stream s t) => Parser s (Maybe t)
peek = optional advance

eof :: (Stream s t) => Parser s ()
eof = do Nothing <- peek
         pure ()

satisfy :: (Stream s t) => (t -> Bool) -> Parser s t
satisfy = flip mfilter advance

expect :: (Stream s t, Eq t) => t -> Parser s ()
expect = void . satisfy . (==)

expects :: (Stream s t, Eq t) => [t] -> Parser s ()
expects = mapM_ expect

oneOf :: (Stream s t, Eq t) => [t] -> Parser s t
oneOf = satisfy . flip elem

noneOf :: (Stream s t, Eq t) => [t] -> Parser s t
noneOf = satisfy . flip notElem

between :: (Stream s t, Eq t) => t -> t -> Parser s a -> Parser s a
between l r par = expect l *> par <* expect r


-- combinators
try :: (Alternative c) => Parser t (c a) -> Parser t (c a)
try = (<|> pure empty)

many1 :: (Alternative f) => f a -> f [a]
many1 par = liftA2 (:) par (many par)

sepBy :: Parser t a -> Parser t () -> Parser t [a]
sepBy par sep = try $ sepBy1 par sep

sepBy1 :: Parser t a -> Parser t () -> Parser t [a]
sepBy1 par sep = liftA2 (:) par rest
  where rest = try $ liftA2 (:) (sep >> par) rest

endBy :: Parser t a -> Parser t () -> Parser t [a]
endBy par sep = many $ par <* sep

leftRec :: Parser t a -> (a -> Parser t a) -> Parser t a
leftRec par fol = par >>= rest
  where rest l = (fol l >>= rest) <|> pure l
