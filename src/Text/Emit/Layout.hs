{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Text.Emit.Layout
  ( module Text.Emit.Layout.Monad,

    -- * TODO
    layout,
    runLayout,
    evalLayout,

    -- * Traversal
    traverseMetadata,
  )
where

import Control.Monad (when)
import Control.Monad.Reader (ask, local)
import Control.Monad.Writer (tell)

import Data.Foldable (traverse_)
import Data.Primitive.Array (Array)
import Data.Text (Text)
import Data.Text qualified as Text

--------------------------------------------------------------------------------

import Text.Emit.Doc
  ( Doc (..),
    JoinDoc (JoinDoc),
    LineDoc (LineDoc),
    MetaDoc (MetaDoc),
    NestDoc (NestDoc),
    TextDoc (TextDoc),
    sizeofDoc,
  )
import Text.Emit.Layout.Monad

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

-- Traversal -------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
traverseMetadata :: Applicative f => (a -> Doc a -> f (Doc b)) -> Doc a -> f (Doc b)
traverseMetadata _ None = pure None
traverseMetadata _ (Line x) = pure (Line x)
traverseMetadata _ (Text x) = pure (Text x)
traverseMetadata k (Join (JoinDoc _ docs)) =
  fmap (Join . rebuild) (traverse (traverseMetadata k) docs)
  where
    rebuild :: Array (Doc a) -> JoinDoc a
    rebuild xs = JoinDoc (foldr ((+) . sizeofDoc) 0 xs) xs
traverseMetadata k (Nest (NestDoc tabs doc)) = fmap (Nest . NestDoc tabs) (traverseMetadata k doc)
traverseMetadata k (Meta (MetaDoc meta doc)) = k meta doc

--------------------------------------------------------------------------------

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
    tell (Text.replicate count (Text.pack "\n") <> Text.replicate i (Text.pack " "))
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
runLayoutNestDoc (NestDoc i x) = do
  local (+ i) do
    tabs <- ask
    tell (Text.cons '\n' (Text.replicate tabs (Text.pack " ")))
    runLayoutDoc x
{-# INLINE runLayoutNestDoc #-}

-- | TODO
--
-- @since 1.0.0
runLayoutMetaDoc :: MetaDoc a -> Layout ()
runLayoutMetaDoc (MetaDoc _ x) = runLayoutDoc x
{-# INLINE runLayoutMetaDoc #-}