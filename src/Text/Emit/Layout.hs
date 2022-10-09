module Text.Emit.Layout
  ( module Text.Emit.Layout.Monad,

    -- * TODO
    runLayout,
    evalLayout,
  )
where

import Data.Text (Text)

--------------------------------------------------------------------------------

import Text.Emit.Layout.Monad

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
runLayout :: Int -> Layout a -> (Text, a)
runLayout i mx =
  case unLayout mx i of
    (# xs, x #) -> (xs, x)
{-# INLINE runLayout #-}

-- | TODO
--
-- @since 1.0.0
evalLayout :: Layout () -> Text
evalLayout mx = fst (runLayout 0 mx)
{-# INLINE evalLayout #-}