{-# LANGUAGE TypeSynonymInstances, OverlappingInstances #-}

module EitherM where

import Data.Either

instance Monad (Either a) where
  return          = Right
  (Right x) >>= f = f x
  (Left x) >>= f  = Left x