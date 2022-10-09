{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Text.Emit
  ( -- * TODO
    Doc (None, Line, Text, Join, Nest, Meta),
    sizeofDoc,

    -- * TODO
    LineDoc (LineDoc),
    sizeofLineDoc,

    -- * TODO
    TextDoc (TextDoc, size, contents),
    sizeofTextDoc,

    -- * TODO
    JoinDoc (JoinDoc, size, docs),
    sizeofJoinDoc,

    -- * TODO
    NestDoc (NestDoc, tabs, doc),
    sizeofNestDoc,

    -- * TODO
    MetaDoc (MetaDoc, meta, doc),
    sizeofMetaDoc,

    -- * TODO
    line,
    text,
    nest,
    metadata,

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

import Control.Monad.ST.Strict (runST)

import Control.Monad.Reader (MonadReader, ask, local)
import Data.Primitive.Array (Array)
import Data.Primitive.Array qualified as Array
import Data.String (IsString, fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Foldable (foldr')

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
line :: Doc a
line = Line (LineDoc 1)
{-# INLINE line #-}

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
layout :: Doc a -> Text 
layout x = evalLayout (runLayoutDoc x) 
{-# INLINE layout #-}

-- | TODO
--
-- @since 1.0.0
runLayout :: Int -> Layout a -> a 
runLayout i (Layout k) = k i
{-# INLINE runLayout #-}

-- | TODO
--
-- @since 1.0.0
evalLayout :: Layout a -> a 
evalLayout = runLayout 0
{-# INLINE evalLayout #-}

-- | TODO
--
-- @since 1.0.0
newtype Layout a = Layout
  {unLayout :: Int -> a}

-- | @since 1.0.0
instance Functor Layout where
  fmap f (Layout k) = Layout (f . k)
  {-# INLINE fmap #-}

-- | @since 1.0.0
instance Applicative Layout where
  pure x = Layout \_ -> x
  {-# INLINE pure #-}

  Layout f <*> Layout g = Layout \i -> (f i) (g i)
  {-# INLINE (<*>) #-}

-- | @since 1.0.0
instance Monad Layout where
  Layout k >>= f = Layout \i -> unLayout (f (k i)) i
  {-# INLINE (>>=) #-}

-- | @since 1.0.0
instance MonadReader Int Layout where
  ask = Layout id
  {-# INLINE ask #-}

  local f (Layout k) = Layout (k . f)
  {-# INLINE local #-}

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
runLayoutDoc :: Doc a -> Layout Text
runLayoutDoc None = pure Text.empty
runLayoutDoc (Line x) = runLayoutLineDoc x
runLayoutDoc (Text x) = runLayoutTextDoc x
runLayoutDoc (Join x) = runLayoutJoinDoc x
runLayoutDoc (Nest x) = runLayoutNestDoc x
runLayoutDoc (Meta x) = runLayoutMetaDoc x
{-# INLINE runLayoutDoc #-}

-- | TODO
--
-- @since 1.0.0
runLayoutLineDoc :: LineDoc -> Layout Text
runLayoutLineDoc (LineDoc count)
  | count < 1 = pure Text.empty
  | otherwise = do 
    i <- ask
    pure (Text.replicate count (Text.pack "\n") <> Text.replicate i (Text.pack " "))
{-# INLINE runLayoutLineDoc #-}

-- | TODO
--
-- @since 1.0.0
runLayoutTextDoc :: TextDoc -> Layout Text
runLayoutTextDoc (TextDoc _ xs) = pure xs
{-# INLINE runLayoutTextDoc #-}

-- | TODO
--
-- @since 1.0.0
runLayoutJoinDoc :: JoinDoc a -> Layout Text
runLayoutJoinDoc (JoinDoc _ xs) = do 
  texts <- traverse runLayoutDoc xs
  pure (foldr' (<>) Text.empty texts)
{-# INLINE runLayoutJoinDoc #-}

-- | TODO
--
-- @since 1.0.0
runLayoutNestDoc :: NestDoc a -> Layout Text
runLayoutNestDoc (NestDoc i x) = local (+ i) (runLayoutDoc x) 
{-# INLINE runLayoutNestDoc #-}

-- | TODO
--
-- @since 1.0.0
runLayoutMetaDoc :: MetaDoc a -> Layout Text
runLayoutMetaDoc (MetaDoc _ x) = runLayoutDoc x
{-# INLINE runLayoutMetaDoc #-}