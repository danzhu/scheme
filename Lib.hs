module Lib (
  zips,
  orErr,
  ) where

import Control.Applicative (Alternative, empty)
import Control.Monad.Except (MonadError, throwError)

zips :: (Alternative f) => [a] -> [b] -> f [(a, b)]
zips (x : xs) (y : ys) = ((x, y) :) <$> zips xs ys
zips [] []             = pure []
zips _ _               = empty

orErr :: (MonadError e m) => Maybe a -> e -> m a
orErr Nothing err  = throwError err
orErr (Just val) _ = pure val
