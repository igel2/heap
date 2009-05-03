{-# LANGUAGE EmptyDataDecls, FlexibleContexts, FlexibleInstances
  , GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeFamilies
  #-}

-- |
module Data.Heap.Item where

import Data.Binary ( Binary )
import Data.Heap.Internal ( Heap )
import Text.Read ( Read(..) )

type MinHeap a = Heap (Prio MinPolicy a) (Val MinPolicy a)
type MaxHeap a = Heap (Prio MaxPolicy a) (Val MaxPolicy a)
type MinPrioHeap prio val = Heap (Prio FstMinPolicy (prio, val)) (Val FstMinPolicy (prio, val))
type MaxPrioHeap prio val = Heap (Prio FstMaxPolicy (prio, val)) (Val FstMaxPolicy (prio, val))

--import Data.Heap.Raw as Raw

---- | A 'Heap' which will always extract the minimum first.
--type MinHeap a = Heap (Priority MinPolicy a) (Value MinPolicy a)
--
---- | A 'Heap' which will always extract the maximum first.
--type MaxHeap a = Heap (Priority MaxPolicy a) (Value MaxPolicy a)
--
---- | A 'Heap' storing priority-value-associations. It only regards the priority
---- for determining the order of elements, the tuple with minimal 'fst' value
---- (i. e. priority) will always be the head of the 'Heap'.
--type MinPrioHeap priority value = Heap FstMinPolicy (priority, value)
--
---- | A 'Heap' storing priority-value-associations. It only regards the priority
---- for determining the order of elements, the tuple with maximal 'fst' value
---- (i. e. priority) will always be the head of the 'Heap'.
--type MaxPrioHeap priority value = Heap FstMaxPolicy (priority, value)


---- | The 'HeapPolicy' class defines an order on the elements contained within
---- a 'Heap'.
----
---- It works almost like the 'Ord' class (especially it has to define a correct
---- mathematical ordering), the only difference is that there are two type
---- parameters. They are needed to enable the type sytem to distinguish between
---- two 'Heap's which each have a different 'HeapPolicy': It prevents errors like
---- this one:
----
---- @
---- let h1 = 'fromFoldable' [1..10] :: MinHeap Int
----     h2 = 'fromFoldable' [1..10] :: MaxHeap Int
----     h3 = 'union' h1 h2 -- we can't form the union of a Min- and a 'MaxHeap'
---- @
--class HeapPolicy p a where
--    -- | Compare two elements, just like 'compare' of the 'Ord' class, so this
--    -- function has to define a mathematical ordering. When using a 'HeapPolicy'
--    -- for a 'Heap', the minimal value (defined by this order) will be the head
--    -- of the 'Heap'.
--    heapCompare :: p -- ^ /Must not be evaluated/.
--        -> a         -- ^ Compared to 3rd parameter.
--        -> a         -- ^ Compared to 2nd parameter.
--        -> Ordering  -- ^ Result of the comparison.

class (Ord (Prio pol item)) => HeapItem pol item where
    data Prio pol item :: *
    type Val  pol item :: *

-- id === (uncurry merge) . split
-- forall p v f. fst (split (merge p (f v)))
--            == fst (split (merge p v))
    split  :: item -> (Prio pol item, Val pol item)
    merge2 :: (Prio pol item, Val pol item) -> item
{-# RULES
--TODO:"merge2/split" forall x. merge2 (split x) = x
"split/merge2" forall x. split (merge2 x) = x
  #-}

merge :: (HeapItem pol item) => Prio pol item -> Val pol item -> item
merge = curry merge2
{-# INLINE merge #-}

---- | Policy type for a 'MinHeap'.
data MinPolicy

instance (Ord a) => HeapItem MinPolicy a where
    newtype Prio MinPolicy a = MinP a deriving (Binary, Eq, Ord)
    type    Val  MinPolicy a = ()

    split  x           = (MinP x, ())
    merge2 (MinP x, _) = x

instance (Read a) => Read (Prio MinPolicy a) where
    readPrec = fmap MinP readPrec
    readListPrec = fmap (fmap MinP) readListPrec

instance (Show a) => Show (Prio MinPolicy a) where
    show (MinP x) = show x

---- | Policy type for a 'MaxHeap'.
data MaxPolicy

instance (Ord a) => HeapItem MaxPolicy a where
    newtype Prio MaxPolicy a = MaxP a deriving (Binary, Eq)
    type    Val  MaxPolicy a = ()

    split  x           = (MaxP x, ())
    merge2 (MaxP x, _) = x

instance (Ord a) => Ord (Prio MaxPolicy a) where
    compare (MaxP x) (MaxP y) = compare y x

instance (Read a) => Read (Prio MaxPolicy a) where
    readPrec = fmap MaxP readPrec
    readListPrec = fmap (fmap MaxP) readListPrec

instance (Show a) => Show (Prio MaxPolicy a) where
    show (MaxP x) = show x

---- | Policy type for a @(priority, value)@ 'MinPrioHeap'.
data FstMinPolicy

instance (Ord prio) => HeapItem FstMinPolicy (prio, val) where
    newtype Prio FstMinPolicy (prio, val) = FMinP prio deriving (Binary, Eq, Ord)
    type    Val  FstMinPolicy (prio, val) = val

    split  (p,       v) = (FMinP p, v)
    merge2 (FMinP p, v) = (p,       v)

instance (Read prio) => Read (Prio FstMinPolicy (prio, val)) where
    readPrec = fmap FMinP readPrec
    readListPrec = fmap (fmap FMinP) readListPrec

instance (Show prio) => Show (Prio FstMinPolicy (prio, val)) where
    show (FMinP x) = show x

---- | Policy type for a @(priority, value)@ 'MaxPrioHeap'.
data FstMaxPolicy

instance (Ord prio) => HeapItem FstMaxPolicy (prio, val) where
    newtype Prio FstMaxPolicy (prio, val) = FMaxP prio deriving (Binary, Eq)
    type    Val  FstMaxPolicy (prio, val) = val

    split  (p,       v) = (FMaxP p, v)
    merge2 (FMaxP p, v) = (p,       v)

instance (Ord prio) => Ord (Prio FstMaxPolicy (prio, val)) where
    compare (FMaxP x) (FMaxP y) = compare y x

instance (Read prio) => Read (Prio FstMaxPolicy (prio, val)) where
    readPrec = fmap FMaxP readPrec
    readListPrec = fmap (fmap FMaxP) readListPrec

instance (Show prio) => Show (Prio FstMaxPolicy (prio, val)) where
    show (FMaxP x) = show x