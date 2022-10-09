{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Text.Emit.Layout.Monad
  ( -- * TODO 
    Layout (Layout, unLayout),
  )
where

import Control.Monad.Reader (MonadReader, ask, local)
import Control.Monad.Writer (MonadWriter, writer, tell, listen, pass)

import Data.Text (Text)
import Data.Text qualified as Text

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
newtype Layout a = Layout
  {unLayout :: Int -> (# Text, a #)}

-- | @since 1.0.0
instance Functor Layout where
  fmap f (Layout k) =
    Layout \i -> case k i of
      (# w, x #) -> (# w, f x #)
  {-# INLINE fmap #-}

-- | @since 1.0.0
instance Applicative Layout where
  pure x = Layout \_ -> (# Text.empty, x #)
  {-# INLINE pure #-}

  Layout f <*> Layout g = 
    Layout \i -> 
      let !(# xs, k #) = f i
          !(# ys, x #) = g i
       in (# Text.append xs ys, k x #)
  {-# INLINE (<*>) #-}

-- | @since 1.0.0
instance Monad Layout where
  Layout k >>= f = 
    Layout \i -> 
      let !(# xs, x #) = k i 
          !(# ys, y #) = unLayout (f x) i
       in (# Text.append xs ys, y #)
  {-# INLINE (>>=) #-}

-- | @since 1.0.0
instance MonadReader Int Layout where
  ask = Layout \i -> (# Text.empty, i #)
  {-# INLINE ask #-}

  local f (Layout k) = Layout \i -> k (f i) 
  {-# INLINE local #-}

-- | @since 1.0.0
instance MonadWriter Text Layout where
  listen (Layout k) = 
    Layout \i -> case k i of 
      (# xs, x #) -> (# xs, (x, xs) #)
  {-# INLINE listen #-}

  pass (Layout k) = 
    Layout \i -> case k i of 
      (# xs, (x, f) #) -> (# f xs, x #)
  {-# INLINE pass #-}

  tell xs = Layout \_ -> (# xs, () #)
  {-# INLINE tell #-}

  writer (x, xs) = Layout \_ -> (# xs, x #)
  {-# INLINE writer #-}
