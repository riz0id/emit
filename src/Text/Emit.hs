{-# OPTIONS_HADDOCK show-extensions #-}

module Text.Emit
  ( module Text.Emit.Class,

    -- * Doc
    Doc,
    layout,

    -- * Primitives
    (<+>),
    (<!>),
    line,
    text,
    nest,
    metadata,

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

import Data.Text (Text)
import Data.Text qualified as Text

--------------------------------------------------------------------------------

import Text.Emit.Class
import Text.Emit.Doc
  ( Doc (..),
    LineDoc (LineDoc),
    MetaDoc (MetaDoc),
    NestDoc (NestDoc),
    TextDoc (TextDoc),
  )
import Text.Emit.Layout (layout)

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
parens x = text (Text.pack "(") <> x <> text (Text.pack ")")
{-# INLINE CONLIKE parens #-}

-- | TODO
--
-- @since 1.0.0
braces :: Doc a -> Doc a
braces x = text (Text.pack "{") <> x <> text (Text.pack "}")
{-# INLINE CONLIKE braces #-}

-- | TODO
--
-- @since 1.0.0
bracks :: Doc a -> Doc a
bracks x = text (Text.pack "[") <> x <> text (Text.pack "]")
{-# INLINE CONLIKE bracks #-}

-- | TODO
--
-- @since 1.0.0
angles :: Doc a -> Doc a
angles x = text (Text.pack "<") <> x <> text (Text.pack ">")
