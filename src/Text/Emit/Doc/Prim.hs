{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Text.Emit.Doc.Prim
  ( -- * Doc
    makeText,
    Step# (Done#, Yield#),
    stream#,
    map#,
    sizeUtf8#,
    unstream#,
    ord2#,
    ord3#,
    ord4#,
    safe#,
  )
where

import GHC.Exts (ByteArray#, Char (C#), Char#, Int (I#), Int#, State#, UnliftedType, Word#, MutableByteArray#)
import GHC.Exts qualified as GHC

import Prelude hiding (lines)
import Data.Text.Internal (Text (Text))
import Data.Text.Array (Array(ByteArray))

--------------------------------------------------------------------------------

makeText :: String -> Text 
makeText str = 
  GHC.runRW# \st# -> 
    case unstream# (map# safe# (stream# str)) st# of 
      (# _, bxs#, n# #) -> Text (ByteArray bxs#) 0 (I# n#)

data Box# :: UnliftedType where
  Box# :: Step# -> Box#

newtype Stream# = Stream# (# Int#, Box# #)

newtype Step# = Step# (# (# #)| (# Int#, Int#, Char#, Box# #) #)

pattern Done# :: Step#
pattern Done# = Step# (# (##) | #)

pattern Yield# :: Int# -> Int# -> Char# -> Step# -> Step#
pattern Yield# off# len# x# xs# = Step# (# | (# off#, len#, x#, Box# xs# #) #)

{-# COMPLETE Done#, Yield# #-}

stream# :: String -> Stream#
stream# str =
  case run# 0# str of
    (# n#, w# #) -> Stream# (# n#, Box# w# #)
  where
    run# :: Int# -> String -> (# Int#, Step# #)
    run# n# [] = (# n#, Done# #)
    run# n# (C# chr# : chrs) =
      let len# :: Int#
          len# = sizeUtf8# chr#
       in case run# (len# GHC.+# n#) chrs of
            (# n'#, nxt# #) -> (# n'#, Yield# n# len# chr# nxt# #)
{-# INLINE stream# #-}

map# :: (Char# -> Char#) -> Stream# -> Stream#
map# f (Stream# (# _, Box# w# #)) =
  case run# 0# w# of
    (# n#, w'# #) -> Stream# (# n#, Box# w'# #)
  where
    run# :: Int# -> Step# -> (# Int#, Step# #)
    run# n# Done# = (# n#, Done# #)
    run# n# (Yield# _ _ chr# nxt#) =
      let !chr'# = f chr#
          !len'# = sizeUtf8# chr'#
       in case run# (len'# GHC.+# n#) nxt# of
            (# n'#, nxt'# #) -> (# n'#, Yield# n# len'# chr'# nxt'# #)
{-# INLINE map# #-}

safe# :: Char# -> Char#
safe# chr# =
  case GHC.andI# (GHC.ord# chr#) 0x1ff800# GHC./=# 0xd800# of
    1# -> chr#
    _ -> '\xfffd'#
{-# INLINE safe# #-}

unstream# :: forall s. Stream# -> State# s -> (# State# s, ByteArray#, Int# #)
unstream# (Stream# (# total'size#, (Box# w#) #)) st0# =
  let !(# st1#, dst# #) = GHC.newByteArray# total'size# st0#
      !st2# = GHC.setByteArray# dst# 0# total'size# 0# st1# 

      loop# :: Step# -> State# s -> State# s
      loop# Done# st# = st#
      loop# (Yield# i# n# chr# nxt#) st# =
        loop# nxt# (writeUtf8# dst# i# n# chr# st#)

      !(# st3#, bxs# #) = GHC.unsafeFreezeByteArray# dst# (loop# w# st2#)
   in (# st3#, bxs#, total'size# #)
{-# INLINE unstream# #-}

writeUtf8# :: MutableByteArray# s -> Int# -> Int# -> Char# -> State# s -> State# s
writeUtf8# dst# i# 1# c# st0# = GHC.writeWord8ArrayAsWord# dst# i# (GHC.int2Word# (GHC.ord# c#)) st0#
writeUtf8# dst# i# 2# c# st0# = GHC.writeWord8ArrayAsWord# dst# i# (ord2# c#) st0#
writeUtf8# dst# i# 3# c# st0# = GHC.writeWord8ArrayAsWord# dst# i# (ord3# c#) st0#
writeUtf8# dst# i# _ c# st0# = GHC.writeWord8ArrayAsWord# dst# i# (ord4# c#) st0#

sizeUtf8# :: Char# -> Int#
sizeUtf8# c# =
  let cmp0# = GHC.geChar# c# '\x80'#
      cmp1# = GHC.geChar# c# '\x800'#
      cmp2# = GHC.geChar# c# '\x10000'#
   in 1# GHC.+# cmp0# GHC.+# cmp1# GHC.+# cmp2#

ord2# :: Char# -> Word#
ord2# chr# = 
  let !word# = GHC.int2Word# (GHC.ord# chr#) 
      !byte1# = GHC.plusWord# (GHC.uncheckedShiftRL# word# 6#) 0xC0## 
      !byte2# = GHC.plusWord# (GHC.and# word# 0x3F##) 0x80## 
   in pack2# byte1# byte2#

pack2# :: Word# -> Word# -> Word# 
pack2# x# y# = GHC.or# x# (GHC.uncheckedShiftL# y# 8#)

ord3# :: Char# -> Word#
ord3# chr# = 
  let !word# = GHC.int2Word# (GHC.ord# chr#) 
      !byte1# = GHC.plusWord# (GHC.uncheckedShiftRL# word# 12#) 0xE0## 
      !byte2# = GHC.plusWord# (GHC.and# (GHC.uncheckedShiftRL# word# 6#) 0x3F##) 0x80## 
      !byte3# = GHC.plusWord# (GHC.and# word# 0x3F##) 0x80## 
   in pack3# byte1# byte2# byte3#

pack3# :: Word# -> Word# -> Word# -> Word# 
pack3# x# y# z# = GHC.or# x# (GHC.uncheckedShiftL# (pack2# y# z#) 8#)

ord4# :: Char# -> Word#
ord4# chr# = 
  let !word# = GHC.int2Word# (GHC.ord# chr#) 
      !byte1# = GHC.plusWord# (GHC.uncheckedShiftRL# word# 18#) 0xF0## 
      !byte2# = GHC.plusWord# (GHC.and# (GHC.uncheckedShiftRL# word# 12#) 0x3F##) 0x80## 
      !byte3# = GHC.plusWord# (GHC.and# (GHC.uncheckedShiftRL# word# 6#) 0x3F##) 0x80## 
      !byte4# = GHC.plusWord# (GHC.and# word# 0x3F##) 0x80## 
   in pack4# byte1# byte2# byte3# byte4#

pack4# :: Word# -> Word# -> Word# -> Word# -> Word#
pack4# x# y# z# w# = GHC.or# (pack2# x# y#) (GHC.uncheckedShiftL# (pack2# z# w#) 16#)
