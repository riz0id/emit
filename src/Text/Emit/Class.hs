{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Text.Emit.Class
  ( -- * TODO
    Emit (emit, emitList),
  )
where

import Data.Foldable (foldl')
import Data.Text (Text)
import Data.Text qualified as Text

--------------------------------------------------------------------------------

import Text.Emit.Doc

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
class Emit a where
  -- | TODO
  --
  -- @since 1.0.0
  emit :: a -> Doc ()

  -- | TODO
  --
  -- @since 1.0.0
  emitList :: [a] -> Doc ()
  emitList [] = None
  emitList (x : xs) =
    Text (TextDoc 1 (Text.pack "["))
      <> emit x
      <> foldl' (\ys y -> ys <> Text (TextDoc 2 (Text.pack ", ")) <> emit y) (Text (TextDoc 1 (Text.pack "]"))) xs
  {-# INLINE emitList #-}

  {-# MINIMAL emit #-}

-- | @since 1.0.0
instance Emit String where
  emit str =
    let text :: Text
        text = Text.pack str
     in Text (TextDoc (Text.length text) text)
  {-# INLINE emit #-}
