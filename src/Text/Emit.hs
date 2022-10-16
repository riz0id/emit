{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Text.Emit
  ( module Text.Emit.Class,

    -- * Doc
    Doc,
    layout,

    -- * Traversal
    traverseMetadata,

    -- * Query
    length,

    -- * Primitives
    (<+>),
    (<!>),
    line,
    text,
    nest,
    metadata,

    -- * Generation
    repeat, 

    -- * Concatenation
    hsep,
    vsep,

    -- * Enclosing
    parens,
    braces,
    bracks,
    angles,
  )
where

import Control.Monad.ST (runST)

import Data.Foldable (for_)
import Data.Primitive.Array qualified as Array
import Data.Text (Text)
import Data.Text qualified as Text

import Prelude hiding (repeat)

--------------------------------------------------------------------------------

import Text.Emit.Class
import Text.Emit.Doc
  ( Doc (Join, Line, Meta, Nest, None, Text),
    JoinDoc (JoinDoc),
    LineDoc (LineDoc),
    MetaDoc (MetaDoc),
    NestDoc (NestDoc),
    TextDoc (TextDoc),
    sizeofDoc,
  )
import Text.Emit.Layout (layout, traverseMetadata)

-- Primitives ------------------------------------------------------------------

infixr 5 <+>, <!>

-- | TODO
--
-- @since 1.0.0
(<+>) :: Doc a -> Doc a -> Doc a
(<+>) x y = x <> text (Text.pack " ") <> y
{-# INLINE CONLIKE (<+>) #-}

-- | TODO
--
-- @since 1.0.0
(<!>) :: Doc a -> Doc a -> Doc a
(<!>) x y = x <> line <> y
{-# INLINE CONLIKE (<!>) #-}

-- | TODO
--
-- @since 1.0.0
line :: Doc a
line = Line (LineDoc 1)
{-# INLINE CONLIKE line #-}

-- | TODO
--
-- @since 1.0.0
text :: Text -> Doc a
text x = Text (TextDoc (Text.length x) x)
{-# INLINE CONLIKE text #-}

-- | TODO
--
-- @since 1.0.0
nest :: Int -> Doc a -> Doc a
nest n x = Nest (NestDoc n x)
{-# INLINE CONLIKE nest #-}

-- | TODO
--
-- @since 1.0.0
metadata :: Doc a -> a -> Doc a
metadata x i = Meta (MetaDoc i x)
{-# INLINE CONLIKE metadata #-}

-- Generation ------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
repeat :: Int -> Doc a -> Doc a
repeat _ None = None
repeat n (Text (TextDoc len txt)) = 
  case compare n 1 of
    LT -> None
    EQ -> Text (TextDoc len txt)
    GT -> Text (TextDoc (n * len) (Text.replicate n txt))
repeat n doc =
  case compare n 1 of
    LT -> None
    EQ -> doc
    GT -> runST do
      dst <- Array.newArray n None
      for_ [0 .. n - 1] \i ->
        Array.writeArray dst i doc
      docs <- Array.unsafeFreezeArray dst
      pure (Join (JoinDoc (n * sizeofDoc doc) docs))

-- Concatenation ---------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
hsep :: [Doc a] -> Doc a
hsep [] = None
hsep [x] = x
hsep (x : xs) = x <+> hsep xs
{-# INLINE hsep #-}

-- | TODO
--
-- @since 1.0.0
vsep :: [Doc a] -> Doc a
vsep [] = None
vsep [x] = x
vsep (x : xs) = x <!> vsep xs
{-# INLINE vsep #-}

-- Enclosing -------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
parens :: Doc a -> Doc a
parens None = text (Text.pack "()")
parens doc = text (Text.pack "(") <> doc <> text (Text.pack ")")
{-# INLINE CONLIKE parens #-}

-- | TODO
--
-- @since 1.0.0
braces :: Doc a -> Doc a
braces None = text (Text.pack "{}")
braces doc = text (Text.pack "{") <> doc <> text (Text.pack "}")
{-# INLINE CONLIKE braces #-}

-- | TODO
--
-- @since 1.0.0
bracks :: Doc a -> Doc a
bracks None = text (Text.pack "[]")
bracks doc = text (Text.pack "[") <> doc <> text (Text.pack "]")
{-# INLINE CONLIKE bracks #-}

-- | TODO
--
-- @since 1.0.0
angles :: Doc a -> Doc a
angles None = text (Text.pack "<>")
angles doc = text (Text.pack "<") <> doc <> text (Text.pack ">")
