{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Record (
        Record, FldProxy(..),
        RecordCons,
        insert, get, modify, set, delete,
        -- merge,
        rnil,
        (&), (:=) ((:=)), (@.)
    ) where

import Data.Aeson (FromJSON(..), ToJSON(..), Object, object, withObject, (.:))
import Data.Aeson.Types (Parser, Value)
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Data.Text (Text, pack)
import GHC.Base (Any, Int(..))
import GHC.Prim
import GHC.ST (ST(..), runST)
import GHC.TypeLits (natVal', Symbol, KnownSymbol, symbolVal)
import GHC.Types (RuntimeRep(TupleRep, LiftedRep))

import Data.Kind.Row (Row, Empty, RowCons, RowLacks, RowUnion, RowNub, FldProxy(..))
import Data.Kind.RowList (RowToList, RowList(..))
import Data.Record.Helpers (RecordCons, getIndex)

data Record (r :: Row Type) = Record (SmallArray# Any)
type role Record representational

data label := value = FldProxy label := !value
infix 8 :=

(&) :: forall l v r1 r2.
    RowLacks l r1 =>
    RecordCons l v r1 r2 =>
    (l := v) -> Record r1 -> Record r2
(&) (lp := v) = insert lp v
infixr 5 &

(@.) :: forall l ty r r'.
    RecordCons l ty r' r =>
    Record r -> FldProxy l -> ty
(@.) r p = get p r
infix 8 @.

instance (
        RowToList r rl,
        RowListToJSON rl r
    ) => ToJSON (Record r) where
        toJSON = object . getValues (Proxy @rl)

class RowListToJSON (rl :: RowList Type) (r :: Row Type) where
    getValues :: Proxy rl -> Record r -> [(Text, Value)]

instance RowListToJSON 'RNil r where
    getValues _ _ = []
instance (
        KnownSymbol s,
        ToJSON v,
        RowListToJSON m r,
        RecordCons s v r' r
    ) => RowListToJSON ('RCons s v m) r where
        getValues _ rec = (pack (symbolVal (Proxy @s)), toJSON (get (FldProxy @s) rec)) : getValues (Proxy @m) rec

instance (RowToList r rl, RowListFromJSON rl r) => FromJSON (Record r) where
    parseJSON = withObject "Record" $ \v -> fromValues (Proxy @rl) v

class RowListFromJSON (rl :: RowList Type) (r :: Row Type) | rl -> r where
    fromValues :: Proxy rl -> Object -> Parser (Record r)

instance RowListFromJSON 'RNil Empty where
    fromValues _ _ = pure rnil
instance (
        KnownSymbol k,
        RowToList r ('RCons k v m),
        RowToList r' m,
        RowLacks k r',
        RowCons k v r' r,
        RowListFromJSON m r',
        FromJSON v
    ) => RowListFromJSON ('RCons k v m) r where
        fromValues _ o = do
                x <- (o .: pack (symbolVal (Proxy @k)) :: Parser v)
                fromValues (Proxy @m) o >>= pure . insert (FldProxy @k) x
        --TODO: Optimize

instance (RowToList r rl, RowListShow rl r) => Show (Record r) where
    show r = "{ " ++ showRec (Proxy @rl) r ++ "}"

class RowListShow (rl :: RowList Type) (r :: Row Type) where
    showRec :: Proxy rl -> Record r -> String

instance RowListShow 'RNil r where
    showRec _ _ = ""

instance {-# OVERLAPPING #-} (
        KnownSymbol k,
        Show v,
        RecordCons k v r' r
    ) => RowListShow ('RCons k v 'RNil) r where
        showRec _ r = showField (Proxy @k) r ++ " "

instance (
        RowListShow m r,
        KnownSymbol k,
        Show v,
        RecordCons k v r' r
    ) => RowListShow ('RCons k v m) r where
        showRec _ r = showField (Proxy @k) r ++ ", " ++ showRec (Proxy @m) r

showField :: forall k v r r'.
             (Show v, KnownSymbol k, RecordCons k v r' r) =>
             Proxy k -> Record r -> String
showField p r = symbolVal p ++ ": " ++ show (get (FldProxy @k) r)

runST' :: (forall s. ST s a) -> a
runST' !s = runST s

rnil :: Record Empty
rnil = runST' $ ST $ \s# ->
        case newSmallArray# 0# (errorWithoutStackTrace "No value") s# of
            (# s'#, arr# #) -> freeze arr# s'#
{-# INLINE rnil #-}

idInt# :: Int# -> Int#
idInt# x# = x#

insert :: forall l ty r1 r2.
    (RowLacks l r1, RecordCons l ty r1 r2) =>
    FldProxy l -> ty -> Record r1 -> Record r2
insert _ x (Record a#) = copyAndInsertNew @r2 i# x f indices a# newSize#
    where newSize# = oldSize# +# 1#
          !oldSize# = sizeofSmallArray# a#
          !(I# i#) = getIndex (Proxy @l) (Proxy @r2)
          indices = [0 .. I# (oldSize# -# 1#)]
          f n# = case n# <# i# of
                0# -> n# +# 1#
                _ -> n#
{-# INLINE insert #-}

modify :: forall l a b r1 r2 r.
    RecordCons l a r r1 =>
    RowCons l b r r2 =>
    FldProxy l -> (a -> b) -> Record r1 -> Record r2
modify _ f (Record a#) = copyAndInsertNew @r2 i# val idInt# indices a# size#
    where !(I# i#) = getIndex (Proxy @l) (Proxy @r1)
          !size# = sizeofSmallArray# a#
          indices = filter (/= I# i#) [0 .. I# (size# -# 1#)]
          (# oldVal #) = indexSmallArray# a# i#
          val = f (unsafeCoerce# oldVal)
{-# INLINE modify #-}

set :: forall l a b r1 r2 r.
    RecordCons l a r r1 =>
    RowCons l b r r2 =>
    FldProxy l -> b -> Record r1 -> Record r2
set p b = modify p (const b)
{-# INLINE set #-}

get :: forall l ty r1 r2.
    RecordCons l ty r1 r2 =>
    FldProxy l -> Record r2 -> ty
get _ (Record arr#) = unsafeCoerce# val
    where (# val #) = indexSmallArray# arr# i#
          !(I# i#) = getIndex (Proxy @l) (Proxy @r2)
{-# INLINE get #-}

delete :: forall l ty r1 r2.
    RowLacks l r2 =>
    RecordCons l ty r2 r1 =>
    FldProxy l -> Record r1 -> Record r2
delete _ (Record arr#) = runST' $ ST $ \s0# ->
        case createAndCopy size# s0# arr# f indices of
            (# s1#, a# #) -> freeze a# s1#
    where size# = sizeofSmallArray# arr# -# 1#
          !(I# i#) = getIndex (Proxy @l) (Proxy @r1)
          indices = filter (/= I# i#) [0 .. I# size#]
          f n# = case n# ># i# of
                0# -> n#
                _ -> n# -# 1#
{-
merge :: forall r1 r2 r3 r4.
    RowUnion r1 r2 r3 =>
    RowNub r3 r4 =>
    Record r1 -> Record r2 -> Record r4
merge (Record a#) (Record b#) = runST' $ ST $ \s0# ->
        case newSmallArray# size# (errorWithoutStackTrace "No value") s0# of
            (# s1#, arr# #) -> case fold'# (applyMapping a# arr#) (# s1#, map1 #) indices1 of
                (# s2#, _ #) -> case fold'# (applyMapping b# arr#) (# s2#, map2 #) indices2 of
                    (# s3#, _ #) -> freeze arr# s3#
    where !(I# size#) = fromIntegral $ natVal' (proxy# :: Proxy# (RowLength r3))
          indices1 = indexList a#
          indices2 = fmap fst map2
          map1 = zip indices1 newIndices1
          map2 = filteredMapping map1 $ zip (indexList b#) newIndices2
          newIndices1 = natVals $ Proxy @(RowIndices r1 r3)
          newIndices2 = natVals $ Proxy @(RowIndices r2 r3)
-}
applyMapping :: SmallArray# Any -> SmallMutableArray# s Any -> Int -> (# State# s, [(Int, Int)] #) -> (# State# s, [(Int, Int)] #)
applyMapping _ _ _ (# _, [] #) = errorWithoutStackTrace "Should not happen"
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
createAndCopy size# s0# arr# f indices = case newSmallArray# size# (errorWithoutStackTrace "No value") s0# of
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
