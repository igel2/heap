{-# LANGUAGE EmptyDataDecls, FlexibleContexts, FlexibleInstances
  , GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeFamilies
  #-}

-- |
module Data.Heap.Item where

import Data.Binary ( Binary )
import Data.Heap.Internal ( Heap )
import Text.Read ( Read(..) )

--TODO: does this make D.H less verbosed *and* more simple?
type ManagedHeap pol item = Heap (Prio pol item) (Val pol item)

-- | A 'Heap' which will always extract the minimum first.
type MinHeap a = ManagedHeap MinPolicy a

-- | A 'Heap' which will always extract the maximum first.
type MaxHeap a = ManagedHeap MaxPolicy a

-- | A 'Heap' storing priority-value pairs @(prio, val)@. The order of elements
-- is solely determined by the priority @prio@, the value @val@ has no influence.
-- The priority-value pair with minmal priority will always be extracted first.
type MinPrioHeap prio val = ManagedHeap FstMinPolicy (prio, val)

-- | A 'Heap' storing priority-value pairs @(prio, val)@. The order of elements
-- is solely determined by the priority @prio@, the value @val@ has no influence.
-- The priority-value pair with maximal priority will always be extracted first.
type MaxPrioHeap prio val = ManagedHeap FstMaxPolicy (prio, val)

-- | @'HeapItem' pol item@ is a type class for items that can be stored in a
-- 'Heap'. A raw @'Heap' prio val@ only provides a minimum priority heap (i. e.
-- @val@ doesn't influence the ordering of elements and the pair with minimal
-- @prio@ will be extracted first, see there). The job of this class is to
-- translate between arbitrary @item@s and priority-value pairs @('Prio' pol
-- item, 'Val' pol item)@, depending on the policy @pol@ to be used, and thus
-- being able to use 'Heap' not only 'MinPrioHeap', but also as 'MinHeap',
-- 'MaxHeap', 'MaxPrioHeap' or a custom implementation. In short: The job of this
-- class is to deconstruct arbitrary @item@s into a @(prio, val)@ pairs that can
-- be handled by a minimum priority heap.
--
-- Example: Consider you want to use @'Heap' prio val@ as a @'MaxHeap' a@. You
-- would have to invert the order of @a@ (e. g. by @newtype InvOrd a = InvOrd a@
-- along with an apropriate 'Ord' instance for it) and then use a @type MaxHeap =
-- 'Heap' (InvOrd a) ()@. You'd also have to translate every @x@ to @(InvOrd x,
-- ())@ before insertion and back after removal in order to retrieve your
-- original type @a@.
--
-- This functionality is provided by the 'HeapItem' class. In the above example,
-- you'd use a 'MaxHeap'. The according instance declaration is of course already
-- provided and looks like this (simplified):
--
-- @data 'MaxPolicy'
--
-- instance (Ord a) => HeapItem MaxPolicy a where
--     newtype 'Prio' 'MaxPolicy' a = MaxP a deriving ('Eq')
--     type    'Val'  'MaxPolicy' a = ()
--     'split' x           = (MaxP x, ())
--     'merge' (MaxP x, _) = x
--
-- instance ('Ord' a) => 'Ord' ('Prio' 'MaxPolicy' a) where
--     'compare' (MaxP x) (MaxP y) = 'compare' y x
-- @
--
-- Where 'MaxPolicy' is a phantom type describing which 'HeapItem' instance is
-- actually meant (we also have 'MinPolicy' for example) and @MaxP@ inverts the
-- ordering of @a@, so that the maximum will be on top of the 'Heap'.
--
-- The conversion functions 'split' and 'merge' have to make sure that
--
-- (1) @forall x. 'merge' ('split' x) == x@ ('merge' and 'split' don't remove,
--     add or alter any information)
--
-- (2) @forall p v f. 'fst' ('split' ('merge' (p, (f v))) == 'fst' ('split'
--     ('merge' (p, v)))@ (modifying the associated value doesn't alter the
--      priority)
class (Ord (Prio pol item)) => HeapItem pol item where
    -- | The part of @item@ that determines the ordering of elements on a 'Heap'.
    data Prio pol item :: *
    -- | Everything not part of @'Prio' pol item@
    type Val  pol item :: *

    -- | Translate an @item@ into a priority-value pair.
    split :: item -> (Prio pol item, Val pol item)
    -- | Restore the @item@ from a priority-value pair.
    merge :: (Prio pol item, Val pol item) -> item
{-# RULES "split/merge" forall x. split (merge x) = x #-}

-- | 'split' a function on @item@s to one on priority-value pairs.
splitF :: (HeapItem pol item) => (item -> a) -> Prio pol item -> Val pol item -> a
splitF f p v = let x = merge (p, v) in f x
{-# INLINE splitF #-}

---- | Policy type for a 'MinHeap'.
data MinPolicy

instance (Ord a) => HeapItem MinPolicy a where
    newtype Prio MinPolicy a = MinP a deriving (Binary, Eq, Ord)
    type    Val  MinPolicy a = ()

    split x           = (MinP x, ())
    merge (MinP x, _) = x

instance (Read a) => Read (Prio MinPolicy a) where
    readPrec     = fmap MinP readPrec
    readListPrec = fmap (fmap MinP) readListPrec

instance (Show a) => Show (Prio MinPolicy a) where
    show (MinP x) = show x

---- | Policy type for a 'MaxHeap'.
data MaxPolicy

instance (Ord a) => HeapItem MaxPolicy a where
    newtype Prio MaxPolicy a = MaxP a deriving (Binary, Eq)
    type    Val  MaxPolicy a = ()

    split x           = (MaxP x, ())
    merge (MaxP x, _) = x

instance (Ord a) => Ord (Prio MaxPolicy a) where
    compare (MaxP x) (MaxP y) = compare y x

instance (Read a) => Read (Prio MaxPolicy a) where
    readPrec     = fmap MaxP readPrec
    readListPrec = fmap (fmap MaxP) readListPrec

instance (Show a) => Show (Prio MaxPolicy a) where
    show (MaxP x) = show x

---- | Policy type for a @(priority, value)@ 'MinPrioHeap'.
data FstMinPolicy

instance (Ord prio) => HeapItem FstMinPolicy (prio, val) where
    newtype Prio FstMinPolicy (prio, val) = FMinP prio deriving (Binary, Eq, Ord)
    type    Val  FstMinPolicy (prio, val) = val

    split (p,       v) = (FMinP p, v)
    merge (FMinP p, v) = (p,       v)

instance (Read prio) => Read (Prio FstMinPolicy (prio, val)) where
    readPrec     = fmap FMinP readPrec
    readListPrec = fmap (fmap FMinP) readListPrec

instance (Show prio) => Show (Prio FstMinPolicy (prio, val)) where
    show (FMinP x) = show x

---- | Policy type for a @(priority, value)@ 'MaxPrioHeap'.
data FstMaxPolicy

instance (Ord prio) => HeapItem FstMaxPolicy (prio, val) where
    newtype Prio FstMaxPolicy (prio, val) = FMaxP prio deriving (Binary, Eq)
    type    Val  FstMaxPolicy (prio, val) = val

    split (p,       v) = (FMaxP p, v)
    merge (FMaxP p, v) = (p,       v)

instance (Ord prio) => Ord (Prio FstMaxPolicy (prio, val)) where
    compare (FMaxP x) (FMaxP y) = compare y x

instance (Read prio) => Read (Prio FstMaxPolicy (prio, val)) where
    readPrec     = fmap FMaxP readPrec
    readListPrec = fmap (fmap FMaxP) readListPrec

instance (Show prio) => Show (Prio FstMaxPolicy (prio, val)) where
    show (FMaxP x) = show x