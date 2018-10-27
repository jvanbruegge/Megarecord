{-# LANGUAGE MagicHash, UnboxedTuples #-}

module Megarecord.Record (
        Record, FldProxy(..),
        insert, get,
        rnil
    ) where

import Data.Typeable (Typeable)
import GHC.ST (ST(..), runST)
import GHC.Base (Any, Int(..))
import GHC.TypeLits (natVal', Symbol)
import GHC.Types (RuntimeRep(TupleRep))
import GHC.OverloadedLabels (IsLabel(..))
import GHC.Prim

import Megarecord.Row (Row, Empty, RowCons, RowLacks)
import Megarecord.Row.Internal (RowIndex, RowInsertIndex)

data Record (r :: Row k) = Record (SmallArray# Any)
type role Record representational

data FldProxy (a :: Symbol) = FldProxy deriving (Show, Typeable)

instance (l ~ l') => IsLabel l (FldProxy l') where
    fromLabel = FldProxy

runST' :: (forall s. ST s a) -> a
runST' !s = runST s

rnil :: Record Empty
rnil = runST' $ ST $ \s# ->
        case newSmallArray# 0# (error "No value") s# of
            (# s'#, arr# #) -> case unsafeFreezeSmallArray# arr# s'# of
                (# s''#, a# #) -> (# s''#, Record a# #)
{-# INLINE rnil #-}

insert :: forall l ty r1 r2 i.
    RowLacks l r1 =>
    RowCons l ty r1 r2 =>
    RowInsertIndex l r1 i =>
    FldProxy l -> ty -> Record r1 -> Record r2
insert _ x (Record a#) = runST' $ ST $ \s0# ->
        case newSmallArray# newSize# (error "No value") s0# of
            (# s1#, arr# #) -> case writeSmallArray# arr# i# (unsafeCoerce# x) s1# of
                s2# -> case fold# (copyElement a# arr# i#) s2# [0 .. I# oldSize# - 1] of
                    s3# -> case unsafeFreezeSmallArray# arr# s3# of
                        (# s4#, ret# #) -> (# s4#, Record ret# #)
    where newSize# = oldSize# +# 1#
          !oldSize# = sizeofSmallArray# a#
          !(I# i#) = fromIntegral $ natVal' (proxy# :: Proxy# i)
{-# INLINE insert #-}

get :: forall l ty r1 r2 i.
    RowCons l ty r1 r2 =>
    RowIndex l r2 i =>
    FldProxy l -> Record r2 -> ty
get _ (Record arr#) = unsafeCoerce# val
    where (# val #) = indexSmallArray# arr# i#
          !(I# i#) = fromIntegral $ natVal' (proxy# :: Proxy# i)
{-# INLINE get #-}

copyElement :: SmallArray# Any -> SmallMutableArray# s Any -> Int# -> Int -> State# s -> State# s
copyElement a# target# skip# !(I# i#) s# = writeSmallArray# target# i'# val s#
    where (# val #) = indexSmallArray# a# i#
          !i'# = case i# <# skip# of
                0# -> i# +# 1#
                _ -> i#

fold# :: forall a (b# :: TYPE ('TupleRep '[])). (a -> b# -> b#) -> b# -> [a] -> b#
fold# _ !s# [] = s#
fold# f !s# (x:xs) = fold# f (f x s#) xs


