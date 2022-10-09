{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Text.Emit.Doc
  ( -- * Doc
    Doc (None, Line, Text, Join, Nest, Meta),
    sizeofDoc,

    -- * LineDoc
    LineDoc (LineDoc),
    sizeofLineDoc,

    -- * TextDoc
    TextDoc (TextDoc, size, contents),
    sizeofTextDoc,

    -- * JoinDoc
    JoinDoc (JoinDoc, size, docs),
    sizeofJoinDoc,

    -- * NestDoc
    NestDoc (NestDoc, tabs, doc),
    sizeofNestDoc,

    -- * MetaDoc
    MetaDoc (MetaDoc, meta, doc),
    sizeofMetaDoc,
  )
where

import Control.Monad.ST.Strict (runST)

import Data.Primitive.Array (Array)
import Data.Primitive.Array qualified as Array
import Data.String (IsString, fromString)
import Data.Text (Text)
import Data.Text qualified as Text

-- Doc -------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data Doc a
  = None
  | Line {-# UNPACK #-} !LineDoc
  | Text {-# UNPACK #-} !TextDoc
  | Join {-# UNPACK #-} !(JoinDoc a)
  | Nest {-# UNPACK #-} !(NestDoc a)
  | Meta {-# UNPACK #-} !(MetaDoc a)

-- | @since 1.0.0
instance Functor Doc where
  fmap _ None = None
  fmap _ (Line n) = Line n
  fmap _ (Text x) = Text x
  fmap f (Join x) = Join (fmap f x)
  fmap f (Nest x) = Nest (fmap f x)
  fmap f (Meta x) = Meta (fmap f x)
  {-# INLINE fmap #-}

-- | @since 1.0.0
instance Semigroup (Doc a) where
  None <> ys = ys
  xs <> None = xs
  Line x <> Line y = Line (x <> y)
  Text x <> Text y = Text (x <> y)
  Join x <> Join y = Join (x <> y)
  x <> y = runST do
    mut <- Array.newArray 2 None
    Array.writeArray mut 0 x
    Array.writeArray mut 1 y
    dxs <- Array.unsafeFreezeArray mut

    pure (Join (JoinDoc (sizeofDoc x + sizeofDoc y) dxs))
  {-# INLINE CONLIKE (<>) #-}

-- | @since 1.0.0
instance Monoid (Doc a) where
  mempty = None
  {-# INLINE CONLIKE mempty #-}

-- | @since 1.0.0
instance IsString (Doc a) where
  fromString xs = Text (TextDoc (length xs) (Text.pack xs))
  {-# INLINE fromString #-}

-- | @since 1.0.0
instance Show a => Show (Doc a) where
  show None = "None"
  show (Line x) = show x
  show (Text x) = show x
  show (Join x) = show x
  show (Nest x) = show x
  show (Meta x) = show x
  {-# INLINE show #-}

-- | TODO
--
-- @since 1.0.0
sizeofDoc :: Doc a -> Int
sizeofDoc None = 0
sizeofDoc (Line x) = sizeofLineDoc x
sizeofDoc (Text x) = sizeofTextDoc x
sizeofDoc (Join x) = sizeofJoinDoc x
sizeofDoc (Nest x) = sizeofNestDoc x
sizeofDoc (Meta x) = sizeofMetaDoc x
{-# INLINE sizeofDoc #-}

-- LineDoc ---------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
newtype LineDoc = LineDoc Int
  deriving (Show)

-- | @since 1.0.0
instance Semigroup LineDoc where
  LineDoc x <> LineDoc y = LineDoc (x + y)
  {-# INLINE (<>) #-}

-- | @since 1.0.0
instance Monoid LineDoc where
  mempty = LineDoc 0
  {-# INLINE mempty #-}

-- | TODO
--
-- @since 1.0.0
sizeofLineDoc :: LineDoc -> Int
sizeofLineDoc (LineDoc n) = n
{-# INLINE sizeofLineDoc #-}

-- TextDoc ---------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data TextDoc = TextDoc
  { size :: {-# UNPACK #-} !Int
  , contents :: Text
  }
  deriving (Show)

-- | @since 1.0.0
instance Semigroup TextDoc where
  TextDoc s0 xs <> TextDoc s1 ys = TextDoc (s0 + s1) (xs <> ys)
  {-# INLINE (<>) #-}

-- | @since 1.0.0
instance Monoid TextDoc where
  mempty = TextDoc 0 Text.empty
  {-# INLINE mempty #-}

-- | TODO
--
-- @since 1.0.0
sizeofTextDoc :: TextDoc -> Int
sizeofTextDoc (TextDoc n _) = n
{-# INLINE sizeofTextDoc #-}

-- JoinDoc ---------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data JoinDoc a = JoinDoc
  { size :: {-# UNPACK #-} !Int
  , docs :: {-# UNPACK #-} !(Array (Doc a))
  }
  deriving (Show)

-- | @since 1.0.0
instance Functor JoinDoc where
  fmap f (JoinDoc s xs) = JoinDoc s (fmap (fmap f) xs)
  {-# INLINE fmap #-}

-- | @since 1.0.0
instance Semigroup (JoinDoc a) where
  JoinDoc s0 xs <> JoinDoc s1 ys = runST do
    let len'xs = Array.sizeofArray xs
    let len'ys = Array.sizeofArray ys

    mut <- Array.newArray (len'xs + len'ys) None
    Array.copyArray mut 0 xs 0 len'xs
    Array.copyArray mut len'xs ys 0 len'ys
    dxs <- Array.unsafeFreezeArray mut

    pure (JoinDoc (s0 + s1) dxs)
  {-# INLINE CONLIKE (<>) #-}

-- | @since 1.0.0
instance Monoid (JoinDoc a) where
  mempty = runST do
    mut <- Array.newArray 0 None
    dxs <- Array.unsafeFreezeArray mut
    pure (JoinDoc 0 dxs)
  {-# INLINE CONLIKE mempty #-}

-- | TODO
--
-- @since 1.0.0
sizeofJoinDoc :: JoinDoc a -> Int
sizeofJoinDoc (JoinDoc n _) = n
{-# INLINE sizeofJoinDoc #-}

-- NestDoc ---------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data NestDoc a = NestDoc
  { tabs :: {-# UNPACK #-} !Int
  , doc :: Doc a
  }
  deriving (Show)

-- | @since 1.0.0
instance Functor NestDoc where
  fmap f (NestDoc n x) = NestDoc n (fmap f x)
  {-# INLINE fmap #-}

-- | @since 1.0.0
instance Semigroup (NestDoc a) where
  NestDoc i x <> NestDoc j y = case compare i j of
    LT -> NestDoc i (x <> Nest (NestDoc (j - i) y))
    EQ -> NestDoc i (x <> y)
    GT -> NestDoc j (Nest (NestDoc (i - j) x) <> y)
  {-# INLINE (<>) #-}

-- | @since 1.0.0
instance Monoid (NestDoc a) where
  mempty = NestDoc 0 None
  {-# INLINE CONLIKE mempty #-}

-- | TODO
--
-- @since 1.0.0
sizeofNestDoc :: NestDoc a -> Int
sizeofNestDoc (NestDoc _ x) = sizeofDoc x
{-# INLINE sizeofNestDoc #-}

-- MetaDoc ---------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data MetaDoc a = MetaDoc
  { meta :: a
  , doc :: Doc a
  }
  deriving (Show)

-- | @since 1.0.0
instance Functor MetaDoc where
  fmap f (MetaDoc i x) = MetaDoc (f i) (fmap f x)
  {-# INLINE fmap #-}

-- | TODO
--
-- @since 1.0.0
sizeofMetaDoc :: MetaDoc a -> Int
sizeofMetaDoc (MetaDoc _ x) = sizeofDoc x
{-# INLINE sizeofMetaDoc #-}