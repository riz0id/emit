{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Text.Emit
  ( -- * Emit
    Emit,
    emit,
    emitList,

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
    none,
    line,
    char,
    string,
    text,
    nest,
    append,
    metadata,

    -- * Generation
    repeat,

    -- * Concatenation
    concats,
    concats',
    sep,
    hsep,
    vsep,

    -- * Enclosing
    parens,
    braces,
    bracks,
    angles,
  )
where

import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text

import Prelude hiding (repeat)

--------------------------------------------------------------------------------

import Text.Emit.Doc
  ( Doc (Join, None, Text),
    JoinDoc (JoinDoc),
    TextDoc (TextDoc),
    append,
    char,
    line,
    metadata,
    nest,
    none,
    string,
    text,
    (<!>),
    (<+>),
    concats,
    concats',
  )
import Text.Emit.Doc qualified as Doc
import Text.Emit.Layout (layout, traverseMetadata)

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
class Emit a where
  -- | TODO
  --
  -- @since 1.0.0
  emit :: a -> Doc x

  -- | TODO
  --
  -- @since 1.0.0
  emitList :: [a] -> Doc x
  emitList docs = bracks (sep (string ", ") (map emit docs))
  {-# INLINE emitList #-}

  {-# MINIMAL emit #-}

-- | @since 1.0.0
instance Emit Integer where
  emit = showing
  {-# INLINE CONLIKE emit #-}

-- | @since 1.0.0
instance Emit Int where
  emit = showing
  {-# INLINE CONLIKE emit #-}

-- | @since 1.0.0
instance Emit Double where
  emit = showing
  {-# INLINE CONLIKE emit #-}

-- | @since 1.0.0
instance Emit Float where
  emit = showing
  {-# INLINE CONLIKE emit #-}

-- | @since 1.0.0
instance Emit Char where
  emit = showing
  {-# INLINE CONLIKE emit #-}

  emitList = string
  {-# INLINE CONLIKE emitList #-}

-- | @since 1.0.0
instance Emit a => Emit [a] where
  emit = emitList
  {-# INLINE CONLIKE emit #-}

-- | @since 1.0.0
instance Emit Text where
  emit = text
  {-# INLINE CONLIKE emit #-}

-- Primitives ------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
showing :: Show a => a -> Doc x
showing x = string (show x)
{-# INLINE CONLIKE showing #-}

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
    GT -> Join (JoinDoc (n * Doc.sizeofDoc doc) (List.replicate n doc))

-- Concatenation ---------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
sep :: Doc a -> [Doc a] -> Doc a
sep _ [] = None
sep None xs = mconcat xs
sep s (doc : docs) = doc <> foldMap (s <>) docs
{-# INLINE sep #-}

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
