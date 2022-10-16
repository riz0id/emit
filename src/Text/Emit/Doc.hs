{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Text.Emit.Doc
  ( -- * Doc
    Doc (None, Line, Text, Join, Nest, Meta),

    -- * Construction
    (<+>),
    (<!>),
    none,
    line,
    lines,
    char,
    string,
    text,
    nest,
    append,
    concats,
    concats',
    metadata,

    -- * Query
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

import Data.Foldable (foldr')
import Data.String (IsString, fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Internal qualified as Text
import Data.Text.Array qualified as Text.Array

import GHC.Exts (Int (I#))
import GHC.Exts qualified as GHC

import Prelude hiding (lines)

--------------------------------------------------------------------------------

import Text.Emit.Doc.Prim qualified as Prim

-- Rewrite Rules ---------------------------------------------------------------

{-# RULES
"doc <> doc -> append doc doc" forall x y. x <> y = append x y
"doc <> doc -> append doc doc" forall x y. x <> y = append x y
  #-}

{-# RULES
"append doc none -> doc" forall x. append x none = x
"append none doc -> doc" forall x. append none x = x
"append lines lines -> lines" forall x y.
  append (lines x) (lines y) =
    lines (x + y)
"append text text -> text" forall xs ys.
  append (text xs) (text ys) =
    text (Text.append xs ys)
"append string string -> text" forall xs ys.
  append (string xs) (string ys) =
    string (xs ++ ys)
"append join join -> join" forall xs ys.
  append (concats xs) (concats ys) =
    concats (xs ++ ys)
  #-}

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
  {-# INLINE CONLIKE [0] fmap #-}

-- | @since 1.0.0
instance Semigroup (Doc a) where
  (<>) = append
  {-# INLINE CONLIKE [0] (<>) #-}

-- | @since 1.0.0
instance Monoid (Doc a) where
  mempty = none
  {-# INLINE CONLIKE [0] mempty #-}

  mconcat = concats 
  {-# INLINE CONLIKE [0] mconcat #-}

-- | @since 1.0.0
instance IsString (Doc a) where
  fromString xs = text (Text.pack xs)
  {-# INLINE CONLIKE [0] fromString #-}

-- | @since 1.0.0
instance Show a => Show (Doc a) where
  show None = "None"
  show (Line x) = show x
  show (Text x) = show x
  show (Join x) = show x
  show (Nest x) = show x
  show (Meta x) = show x
  {-# INLINE show #-}

-- Doc - Construction ----------------------------------------------------------

infixr 5 <+>, <!>, `append`

-- | TODO
--
-- @since 1.0.0
(<+>) :: Doc a -> Doc a -> Doc a
(<+>) x y = x <> text (Text.pack " ") <> y
{-# INLINE CONLIKE [0] (<+>) #-}

-- | TODO
--
-- @since 1.0.0
(<!>) :: Doc a -> Doc a -> Doc a
(<!>) x y = x <> line <> y
{-# INLINE CONLIKE [0] (<!>) #-}

-- | TODO
--
-- @since 1.0.0
none :: Doc a
none = None
{-# INLINE CONLIKE [0] none #-}

-- | TODO
--
-- @since 1.0.0
line :: Doc a
line = lines 1
{-# INLINE CONLIKE [0] line #-}

-- | TODO
--
-- @since 1.0.0
lines :: Int -> Doc a
lines n = Line (LineDoc n)
{-# INLINE CONLIKE [0] lines #-}

-- | TODO
--
-- @since 1.0.0
char :: Char -> Doc a
char x = Text (TextDoc 1 (Text.singleton x))
{-# INLINE CONLIKE [0] char #-}

-- | TODO
--
-- @since 1.0.0
string :: String -> Doc a
string str = 
  GHC.runRW# \st# -> 
    case Prim.unstream# (Prim.map# Prim.safe# (Prim.stream# str)) st# of 
      (# _, bxs#, n# #) -> Text (TextDoc (I# n#) (Text.Text (Text.Array.ByteArray bxs#) 0 (I# n#)))
{-# INLINE CONLIKE [0] string #-}

-- | TODO
--
-- @since 1.0.0
text :: Text -> Doc a
text x = Text (TextDoc (Text.length x) x)
{-# INLINE CONLIKE [0] text #-}

-- | TODO
--
-- @since 1.0.0
nest :: Int -> Doc a -> Doc a
nest n x = Nest (NestDoc n x)
{-# INLINE CONLIKE [0] nest #-}

-- | TODO
--
-- @since 1.0.0
append :: Doc a -> Doc a -> Doc a
append None ys = ys
append xs None = xs
append (Line x) (Line y) = Line (x <> y)
append (Text x) (Text y) = Text (x <> y)
append (Join x) (Join y) = Join (x <> y)
append x y = Join (JoinDoc (sizeofDoc x + sizeofDoc y) [x, y])
{-# INLINE CONLIKE [0] append #-}

-- | TODO
--
-- @since 1.0.0
concats :: [Doc a] -> Doc a
concats = foldr append None
{-# INLINE CONLIKE [0] concats #-}

-- | TODO
--
-- @since 1.0.0
concats' :: [Doc a] -> Doc a
concats' = foldr' append None
{-# INLINE CONLIKE [0] concats' #-}

-- | TODO
--
-- @since 1.0.0
metadata :: Doc a -> a -> Doc a
metadata x i = Meta (MetaDoc i x)
{-# INLINE CONLIKE [0] metadata #-}

-- Doc - Query -----------------------------------------------------------------

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
  {-# INLINE CONLIKE [0] (<>) #-}

-- | @since 1.0.0
instance Monoid LineDoc where
  mempty = LineDoc 0
  {-# INLINE CONLIKE [0] mempty #-}

-- | TODO
--
-- @since 1.0.0
sizeofLineDoc :: LineDoc -> Int
sizeofLineDoc (LineDoc n) = n
{-# INLINE CONLIKE [0] sizeofLineDoc #-}

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
  {-# INLINE CONLIKE [0] (<>) #-}

-- | @since 1.0.0
instance Monoid TextDoc where
  mempty = TextDoc 0 Text.empty
  {-# INLINE CONLIKE [0] mempty #-}

-- | TODO
--
-- @since 1.0.0
sizeofTextDoc :: TextDoc -> Int
sizeofTextDoc (TextDoc n _) = n
{-# INLINE CONLIKE [0] sizeofTextDoc #-}

-- JoinDoc ---------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data JoinDoc a = JoinDoc
  { size :: {-# UNPACK #-} !Int
  , docs :: [Doc a]
  }
  deriving (Show)

-- | @since 1.0.0
instance Functor JoinDoc where
  fmap f (JoinDoc s xs) = JoinDoc s (fmap (fmap f) xs)
  {-# INLINE CONLIKE [0] fmap #-}

-- | @since 1.0.0
instance Semigroup (JoinDoc a) where
  JoinDoc s0 xs <> JoinDoc s1 ys = JoinDoc (s0 + s1) (xs ++ ys)
  {-# INLINE CONLIKE [0] (<>) #-}

-- | @since 1.0.0
instance Monoid (JoinDoc a) where
  mempty = JoinDoc 0 []
  {-# INLINE CONLIKE [0] mempty #-}

-- | TODO
--
-- @since 1.0.0
sizeofJoinDoc :: JoinDoc a -> Int
sizeofJoinDoc (JoinDoc n _) = n
{-# INLINE CONLIKE [0] sizeofJoinDoc #-}

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
  {-# INLINE CONLIKE [0] fmap #-}

-- | @since 1.0.0
instance Semigroup (NestDoc a) where
  NestDoc i x <> NestDoc j y = case compare i j of
    LT -> NestDoc i (x <> Nest (NestDoc (j - i) y))
    EQ -> NestDoc i (x <> y)
    GT -> NestDoc j (Nest (NestDoc (i - j) x) <> y)
  {-# INLINE CONLIKE [0] (<>) #-}

-- | @since 1.0.0
instance Monoid (NestDoc a) where
  mempty = NestDoc 0 None
  {-# INLINE CONLIKE [0] mempty #-}

-- | TODO
--
-- @since 1.0.0
sizeofNestDoc :: NestDoc a -> Int
sizeofNestDoc (NestDoc _ x) = sizeofDoc x
{-# INLINE CONLIKE [0] sizeofNestDoc #-}

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
  {-# INLINE CONLIKE [0] fmap #-}

-- | TODO
--
-- @since 1.0.0
sizeofMetaDoc :: MetaDoc a -> Int
sizeofMetaDoc (MetaDoc _ x) = sizeofDoc x
{-# INLINE CONLIKE [0] sizeofMetaDoc #-}