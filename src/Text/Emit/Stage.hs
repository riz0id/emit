{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Text.Emit.Stage
  ( -- * Staging
    stageDoc,
    stageLineDoc,
    stageTextDoc,
    stageJoinDoc,
    stageNestDoc,
    stageMetaDoc,


    -- * StageDoc
    StageDoc (StageNone, StageText, StageLine, StageMeta),

    -- ** Fusion
    fuseStageDoc,

    -- * StageLineDoc
    StageLineDoc (StageLineDoc, size, next),

    -- ** Fusion
    fuseStageLineDoc,

    -- * StageTextDoc
    StageTextDoc (StageTextDoc, size, text, next),

    -- ** Fusion
    fuseStageTextDoc,

    -- * StageMetaDoc
    StageMetaDoc (StageMetaDoc, meta, next),

    -- ** Fusion
    fuseStageMetaDoc,
  )
where

import Data.Foldable (foldMap', foldr')
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
  )
import Text.Emit.Doc qualified as Doc

-- Staging ---------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
stageDoc :: Doc a -> StageDoc a -> StageDoc a
stageDoc None = id
stageDoc (Line x) = stageLineDoc x
stageDoc (Text x) = stageTextDoc x
stageDoc (Join x) = stageJoinDoc x
stageDoc (Nest x) = stageNestDoc x
stageDoc (Meta x) = stageMetaDoc x
{-# INLINE stageDoc #-}

-- | TODO
--
-- @since 1.0.0
stageLineDoc :: LineDoc -> StageDoc a -> StageDoc a
stageLineDoc (LineDoc n) nxt = StageLine (StageLineDoc n nxt)
{-# INLINE CONLIKE stageLineDoc #-}

-- | TODO
--
-- @since 1.0.0
stageTextDoc :: TextDoc -> StageDoc a -> StageDoc a
stageTextDoc (TextDoc s xs) nxt = StageText (StageTextDoc s xs nxt)
{-# INLINE CONLIKE stageTextDoc #-}

-- | TODO
--
-- @since 1.0.0
stageJoinDoc :: JoinDoc a -> StageDoc a -> StageDoc a
stageJoinDoc (JoinDoc _ xs) = foldr' ((.) . stageDoc) id xs
{-# INLINE stageJoinDoc #-}

-- | TODO
--
-- @since 1.0.0
stageNestDoc :: NestDoc a -> StageDoc a -> StageDoc a
stageNestDoc (NestDoc _ None) = stageDoc None
stageNestDoc (NestDoc n (Line doc)) =
  let padding :: TextDoc
      padding = TextDoc n (Text.replicate n (Text.pack " "))
   in stageLineDoc doc . stageTextDoc padding
stageNestDoc (NestDoc _ (Text doc)) = stageTextDoc doc
stageNestDoc (NestDoc n (Join doc)) =
  let padding :: Doc a
      padding = Text (TextDoc n (Text.replicate n (Text.pack " ")))

      pad :: Doc a -> Doc a
      pad None = None
      pad (Line x) = Line x <> padding
      pad (Text x) = Text x
      pad (Join x) = foldMap' pad x.docs
      pad (Nest x) = Nest (NestDoc x.tabs (pad x.doc))
      pad (Meta x) = Meta (MetaDoc x.meta (pad x.doc))
   in stageDoc (foldMap' pad doc.docs)
stageNestDoc (NestDoc n (Nest doc)) =
  let tabs' :: Int
      tabs' = n + doc.tabs
   in stageNestDoc (NestDoc tabs' doc.doc)
stageNestDoc (NestDoc n (Meta doc)) =
  stageMetaDoc (MetaDoc doc.meta (Nest (NestDoc n doc.doc)))
{-# INLINE stageNestDoc #-}

-- | TODO
--
-- @since 1.0.0
stageMetaDoc :: MetaDoc a -> StageDoc a -> StageDoc a
stageMetaDoc (MetaDoc i x) nxt = StageMeta (StageMetaDoc i (stageDoc x nxt))
{-# INLINE stageMetaDoc #-}

-- StageDoc --------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data StageDoc a
  = StageNone
  | StageLine {-# UNPACK #-} !(StageLineDoc a)
  | StageText {-# UNPACK #-} !(StageTextDoc a)
  | StageMeta {-# UNPACK #-} !(StageMetaDoc a)

-- | @since 1.0.0
instance Show a => Show (StageDoc a) where
  show StageNone = "StageNone"
  show (StageText x) = show x
  show (StageLine x) = show x
  show (StageMeta x) = show x
  {-# INLINE show #-}

-- StageDoc - Fusing -----------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
fuseStageDoc :: StageDoc a -> StageDoc a
fuseStageDoc StageNone = StageNone
fuseStageDoc (StageLine x) = StageLine (fuseStageLineDoc x)
fuseStageDoc (StageText x) = StageText (fuseStageTextDoc x)
fuseStageDoc (StageMeta x) = StageMeta (fuseStageMetaDoc x)

-- StageLineDoc -----------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data StageLineDoc a = StageLineDoc
  { size :: {-# UNPACK #-} !Int
  , next :: StageDoc a
  }
  deriving (Show)

-- StageLineDoc - Fusion -------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
fuseStageLineDoc :: StageLineDoc a -> StageLineDoc a
fuseStageLineDoc doc =
  case fuseStageDoc doc.next of
    StageLine doc' -> StageLineDoc (doc.size + doc'.size) doc'.next
    other -> StageLineDoc doc.size other
{-# INLINE fuseStageLineDoc #-}

-- StageTextDoc -----------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data StageTextDoc a = StageTextDoc
  { size :: {-# UNPACK #-} !Int
  , text :: {-# UNPACK #-} !Text
  , next :: StageDoc a
  }
  deriving (Show)

-- StageTextDoc - Fusion -------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
fuseStageTextDoc :: StageTextDoc a -> StageTextDoc a
fuseStageTextDoc doc =
  case fuseStageDoc doc.next of
    StageText doc' ->
      let size' = doc.size + doc'.size
          text' = Text.append doc.text doc'.text
       in StageTextDoc size' text' doc'.next
    other -> StageTextDoc doc.size doc.text other
{-# INLINE fuseStageTextDoc #-}

-- StageMetaDoc -----------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data StageMetaDoc a = StageMetaDoc
  { meta :: a
  , next :: StageDoc a
  }
  deriving (Show)

-- StageMetaDoc - Fusion -------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
fuseStageMetaDoc :: StageMetaDoc a -> StageMetaDoc a
fuseStageMetaDoc doc = StageMetaDoc doc.meta (fuseStageDoc doc.next)
{-# INLINE fuseStageMetaDoc #-}