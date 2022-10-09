{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}

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

    -- * TODO
    Layout (Layout, unLayout),
    layout,
    runLayout,
    evalLayout,

    -- * TODO
    runLayoutDoc,
    runLayoutLineDoc,
    runLayoutTextDoc,
    runLayoutJoinDoc,
    runLayoutNestDoc,
    runLayoutMetaDoc,
  )
where

import Control.Monad.Reader (ask, local)

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Foldable (foldr', traverse_)

--------------------------------------------------------------------------------

import Text.Emit.Doc
import Text.Emit.Layout
import Control.Monad.Writer (tell)
import Control.Monad (when)

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
{-# INLINE paren #-}

-- | TODO
--
-- @since 1.0.0
brace :: Doc a -> Doc a 
brace x = text (Text.pack "{") <> x <> text (Text.pack "}")
{-# INLINE brace #-}

-- | TODO
--
-- @since 1.0.0
brack :: Doc a -> Doc a 
brack x = text (Text.pack "[") <> x <> text (Text.pack "]")
{-# INLINE brack #-}

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
layout :: Doc a -> Text 
layout x = evalLayout (runLayoutDoc x) 
{-# INLINE layout #-}

-- | TODO
--
-- @since 1.0.0
runLayoutDoc :: Doc a -> Layout ()
runLayoutDoc None = pure () 
runLayoutDoc (Line x) = runLayoutLineDoc x
runLayoutDoc (Text x) = runLayoutTextDoc x
runLayoutDoc (Join x) = runLayoutJoinDoc x
runLayoutDoc (Nest x) = runLayoutNestDoc x
runLayoutDoc (Meta x) = runLayoutMetaDoc x
{-# INLINE runLayoutDoc #-}

-- | TODO
--
-- @since 1.0.0
runLayoutLineDoc :: LineDoc -> Layout ()
runLayoutLineDoc (LineDoc count) = 
  when (0 < count) do 
    i <- ask
    tell (Text.replicate count (Text.pack "\n"))
    tell (Text.replicate i (Text.pack " "))
{-# INLINE runLayoutLineDoc #-}

-- | TODO
--
-- @since 1.0.0
runLayoutTextDoc :: TextDoc -> Layout ()
runLayoutTextDoc (TextDoc _ xs) = tell xs
{-# INLINE runLayoutTextDoc #-}

-- | TODO
--
-- @since 1.0.0
runLayoutJoinDoc :: JoinDoc a -> Layout ()
runLayoutJoinDoc (JoinDoc _ xs) = traverse_ runLayoutDoc xs
{-# INLINE runLayoutJoinDoc #-}

-- | TODO
--
-- @since 1.0.0
runLayoutNestDoc :: NestDoc a -> Layout ()
runLayoutNestDoc (NestDoc i x) = local (+ i) (runLayoutDoc x) 
{-# INLINE runLayoutNestDoc #-}

-- | TODO
--
-- @since 1.0.0
runLayoutMetaDoc :: MetaDoc a -> Layout ()
runLayoutMetaDoc (MetaDoc _ x) = runLayoutDoc x
{-# INLINE runLayoutMetaDoc #-}