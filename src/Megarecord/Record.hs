{-# LANGUAGE MagicHash, UnboxedTuples #-}

module Megarecord.Record (
        Record, FldProxy(..),
        insert, get, modify, set, delete,
        merge,
        rnil
    ) where

import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable)
import GHC.ST (ST(..), runST)
import GHC.Base (Any, Int(..))
import GHC.TypeLits (natVal', Symbol)
import GHC.Types (RuntimeRep(TupleRep, LiftedRep))
import GHC.OverloadedLabels (IsLabel(..))
import GHC.Prim

import Megarecord.Row (Row, Empty, RowCons, RowLacks, RowUnion, RowNub)
import Megarecord.Row.Internal (RowIndex, RowIndices, RowLength, natVals)

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

idInt# :: Int# -> Int#
idInt# x# = x#

insert :: forall i l ty r1 r2.
    RowLacks l r1 =>
    RowCons l ty r1 r2 =>
    RowIndex l r2 i =>
    FldProxy l -> ty -> Record r1 -> Record r2
insert _ x (Record a#) = copyAndInsertNew @r2 i# x f indices a# newSize#
    where newSize# = oldSize# +# 1#
          !oldSize# = sizeofSmallArray# a#
          !(I# i#) = fromIntegral $ natVal' (proxy# :: Proxy# i)
          indices = [0 .. I# (oldSize# -# 1#)]
          f n# = case n# <# i# of
                0# -> n# +# 1#
                _ -> n#
{-# INLINE insert #-}

modify :: forall l a b r1 r2 r i.
    RowCons l a r r1 =>
    RowCons l b r r2 =>
    RowIndex l r1 i =>
    FldProxy l -> (a -> b) -> Record r1 -> Record r2
modify _ f (Record a#) = copyAndInsertNew @r2 i# val idInt# indices a# size#
    where !(I# i#) = fromIntegral $ natVal' (proxy# :: Proxy# i)
          !size# = sizeofSmallArray# a#
          indices = filter (/= I# i#) [0 .. I# (size# -# 1#)]
          (# oldVal #) = indexSmallArray# a# i#
          val = f (unsafeCoerce# oldVal)
{-# INLINE modify #-}

set :: forall l a b r1 r2 r i.
    RowCons l a r r1 =>
    RowCons l b r r2 =>
    RowIndex l r1 i =>
    FldProxy l -> b -> Record r1 -> Record r2
set p b = modify p (const b)
{-# INLINE set #-}

get :: forall l ty r1 r2 i.
    RowCons l ty r1 r2 =>
    RowIndex l r2 i =>
    FldProxy l -> Record r2 -> ty
get _ (Record arr#) = unsafeCoerce# val
    where (# val #) = indexSmallArray# arr# i#
          !(I# i#) = fromIntegral $ natVal' (proxy# :: Proxy# i)
{-# INLINE get #-}

delete :: forall l ty r1 r2 i.
    RowLacks l r2 =>
    RowCons l ty r2 r1 =>
    RowIndex l r1 i =>
    FldProxy l -> Record r1 -> Record r2
delete _ (Record arr#) = runST' $ ST $ \s0# ->
        case createAndCopy size# s0# arr# f indices of
            (# s1#, a# #) -> freeze a# s1#
    where size# = sizeofSmallArray# arr# -# 1#
          !(I# i#) = fromIntegral $ natVal' (proxy# :: Proxy# i)
          indices = filter (/= I# i#) [0 .. I# size#]
          f n# = case n# ># i# of
                0# -> n#
                _ -> n# -# 1#

merge :: forall r1 r2 r3 r4 idx1 idx2 n.
    RowUnion r1 r2 r3 =>
    RowNub r3 r4 =>
    RowIndices r1 r4 idx1 =>
    RowIndices r2 r4 idx2 =>
    RowLength r4 n =>
    Record r1 -> Record r2 -> Record r4
merge (Record a#) (Record b#) = runST' $ ST $ \s0# ->
        case newSmallArray# size# (error "No value") s0# of
            (# s1#, arr# #) -> case fold'# (applyMapping a# arr#) (# s1#, map1 #) indices1 of
                (# s2#, _ #) -> case fold'# (applyMapping b# arr#) (# s2#, map2 #) indices2 of
                    (# s3#, _ #) -> freeze arr# s3#
    where !(I# size#) = fromIntegral $ natVal' (proxy# :: Proxy# n)
          indices1 = indexList a#
          indices2 = fmap fst map2
          map1 = zip indices1 newIndices1
          map2 = filteredMapping map1 $ zip (indexList b#) newIndices2
          newIndices1 = natVals $ Proxy @idx1
          newIndices2 = natVals $ Proxy @idx2

applyMapping :: SmallArray# Any -> SmallMutableArray# s Any -> Int -> (# State# s, [(Int, Int)] #) -> (# State# s, [(Int, Int)] #)
applyMapping _ _ _ (# _, [] #) = error "Should not happen"
applyMapping a# target# _ (# s#, (I# x#, I# y#):xs #) = (# writeSmallArray# target# y# val s#, xs #)
    where (# val #) = indexSmallArray# a# x#

filteredMapping :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
filteredMapping [] x = x
filteredMapping _ [] = []
filteredMapping a@((_, y):xs) b@(c@(_, y'):ys)
        | y < y' = filteredMapping xs b
        | y == y' = filteredMapping xs ys
        | otherwise = c : filteredMapping a ys

indexList :: SmallArray# Any -> [Int]
indexList a# = [0 .. I# (sizeofSmallArray# a# -# 1#)]

copyAndInsertNew :: forall r ty. Int# -> ty -> (Int# -> Int#) -> [Int] -> SmallArray# Any -> Int# -> Record r
copyAndInsertNew i# x f indices arr# size# = runST' $ ST $ \s0# ->
        case createAndCopy size# s0# arr# f indices of
            (# s1#, a# #) -> case writeSmallArray# a# i# (unsafeCoerce# x) s1# of
                s2# -> freeze a# s2#

createAndCopy :: Int# -> State# s -> SmallArray# Any -> (Int# -> Int#) -> [Int] -> (# State# s, SmallMutableArray# s Any #)
createAndCopy size# s0# arr# f indices = case newSmallArray# size# (error "No value") s0# of
                (# s1#, a# #) -> case fold# (copyElement arr# a# f) s1# indices of
                    s2# -> (# s2#, a# #)

freeze :: forall r s. SmallMutableArray# s Any -> State# s -> (# State# s, Record r #)
freeze arr# s# = case unsafeFreezeSmallArray# arr# s# of
        (# s'#, !ret# #) -> (# s'#, Record ret# #)

copyElement :: SmallArray# Any -> SmallMutableArray# s Any -> (Int# -> Int#) -> Int -> State# s -> State# s
copyElement a# target# f !(I# i#) s# = writeSmallArray# target# (f i#) val s#
    where (# val #) = indexSmallArray# a# i#

fold# :: forall a (b# :: TYPE ('TupleRep '[])). (a -> b# -> b#) -> b# -> [a] -> b#
fold# _ !s# [] = s#
fold# f !s# (x:xs) = fold# f (f x s#) xs

fold'# :: forall a (b# :: TYPE ('TupleRep '[ 'TupleRep '[], 'LiftedRep])). (a -> b# -> b#) -> b# -> [a] -> b#
fold'# _ !s# [] = s#
fold'# f !s# (x:xs) = fold'# f (f x s#) xs
