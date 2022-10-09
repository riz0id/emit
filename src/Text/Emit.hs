module Text.Emit
  ( -- * TODO
    (<+>),
    (<!>),
    line,
    text,
    nest,
    metadata,

    -- * TODO
    csep,
    hsep,
    vsep,

    -- * TODO
    paren,
    brace,
    brack,
  )
where

import Data.Foldable (foldr')
import Data.Text (Text)
import Data.Text qualified as Text

--------------------------------------------------------------------------------

import Text.Emit.Doc
  ( Doc (..),
    LineDoc (LineDoc),
    MetaDoc (MetaDoc),
    NestDoc (NestDoc),
    TextDoc (TextDoc),
  )

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
csep :: [Doc a] -> Doc a
csep = foldr' (<>) None
{-# INLINE csep #-}

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

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
paren :: Doc a -> Doc a
paren x = text (Text.pack "(") <> x <> text (Text.pack ")")
{-# INLINE CONLIKE paren #-}

-- | TODO
--
-- @since 1.0.0
brace :: Doc a -> Doc a
brace x = text (Text.pack "{") <> x <> text (Text.pack "}")
{-# INLINE CONLIKE brace #-}

-- | TODO
--
-- @since 1.0.0
brack :: Doc a -> Doc a
brack x = text (Text.pack "[") <> x <> text (Text.pack "]")
{-# INLINE CONLIKE brack #-}
