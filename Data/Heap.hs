{-# LANGUAGE CPP, DeriveDataTypeable, EmptyDataDecls, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}

-- | A flexible implementation of min-, max- and custom-priority heaps based on
-- the leftist-heaps from Chris Okasaki's book \"Purely Functional Data
-- Structures\", Cambridge University Press, 1998, chapter 3.1.
--
-- There are different flavours of 'Heap's, each of them following a different
-- strategy when ordering its elements:
--
--  * Choose 'MinHeap' or 'MaxHeap' if you need a simple minimum or maximum heap
--    (which always keeps the minimum/maximum element at the head of the 'Heap').
--
--  * If you wish to manually annotate a value with a priority, e. g. an @IO ()@
--    action with an 'Int' use 'MinPrioHeap' or 'MaxPrioHeap'. They manage
--    @(priority, value)@ tuples so that only the priority (and not the value)
--    influences the order of elements.
--
--  * If you still need something different, define a custom order for the heap
--    elements by implementing a 'HeapPolicy' and let the maintainer know,
--    what's missing.
module Data.Heap
    ( -- * Types
      -- ** Various heap flavours
#ifdef __DEBUG__
      Heap(..), rank, policy
#else
      Heap
#endif
    , MinHeap, MaxHeap, MinPrioHeap, MaxPrioHeap
      -- ** Ordering policies
    , HeapPolicy(..), MinPolicy, MaxPolicy, FstMinPolicy, FstMaxPolicy
      -- * Query
    , null, isEmpty, size, view
      -- ** Unsafe queries
    , unsafeHead, unsafeTail, unsafeUncons
      -- * Construction
    , empty, singleton, insert, union, unions
      -- * Filter
    , filter, partition
      -- * Subranges
    , take, drop, splitAt
    , takeWhile, dropWhile, span, break
      -- * Conversion
      -- ** Foldable
    , fromFoldable, fromAscFoldable, fromDescFoldable
      -- ** List
    , toList, toAscList, toDescList
    ) where

import Control.Exception ( assert )
import Data.Binary ( Binary(..) )
import Data.Foldable ( Foldable, foldl' )
import qualified Data.Foldable as Foldable ( toList )
import Data.List ( sortBy )
import Data.Monoid ( Monoid(..) )
import Data.Ord ( comparing )
import Data.Typeable ( Typeable )
import Prelude hiding
    ( break, drop, dropWhile, filter, null, span, splitAt, take, takeWhile )

-- | The basic 'Heap' type.
data Heap p a
    = Empty -- rank, size, elem, left, right
    | Tree {-# UNPACK #-} !Int {-# UNPACK #-} !Int a !(Heap p a) !(Heap p a)
    deriving ( Typeable )

-- | A 'Heap' which will always extract the minimum first.
type MinHeap = Heap MinPolicy

-- | A 'Heap' which will always extract the maximum first.
type MaxHeap = Heap MaxPolicy

-- | A 'Heap' storing priority-value-associations. It only regards the priority
-- for determining the order of elements, the tuple with minimal 'fst' value
-- (i. e. priority) will always be the head of the 'Heap'.
type MinPrioHeap priority value = Heap FstMinPolicy (priority, value)

-- | A 'Heap' storing priority-value-associations. It only regards the priority
-- for determining the order of elements, the tuple with maximal 'fst' value
-- (i. e. priority) will always be the head of the 'Heap'.
type MaxPrioHeap priority value = Heap FstMaxPolicy (priority, value)

instance (Show a) => Show (Heap p a) where
    show = ("fromList " ++) . show . toList

instance (HeapPolicy p a, Read a) => Read (Heap p a) where
    readsPrec p = readParen (p > 10) $ \r -> do
        ("fromList", s) <- lex r
        (xs, t)         <- reads s
        return ((fromFoldable :: [a] -> Heap p a) xs, t)

instance (HeapPolicy p a) => Eq (Heap p a) where
    h1 == h2 = EQ == compare h1 h2

instance (HeapPolicy p a) => Ord (Heap p a) where
    compare h1 h2 = compareBy (heapCompare (policy h1)) (toAscList h1) (toAscList h2)
        where
        compareBy :: (a -> a -> Ordering) -> [a] -> [a] -> Ordering
        compareBy _   []     []     = EQ
        compareBy _   []     _      = LT
        compareBy _   _      []     = GT
        compareBy cmp (x:xs) (y:ys) = mappend (cmp x y) (compareBy cmp xs ys)

instance (HeapPolicy p a) => Monoid (Heap p a) where
    mempty  = empty
    mappend = union
    mconcat = unions

instance (HeapPolicy p a, Binary a) => Binary (Heap p a) where
    put = put . toDescList
    get = fmap (fromDescFoldable :: [a] -> Heap p a) get

-- | The 'HeapPolicy' class defines an order on the elements contained within
-- a 'Heap'.
class HeapPolicy p a where
    -- | Compare two elements, just like 'compare' of the 'Ord' class, so this
    -- function has to define a mathematical ordering. When using a 'HeapPolicy'
    -- for a 'Heap', the minimal value (defined by this order) will be the head
    -- of the 'Heap'.
    heapCompare :: p -- ^ /Must not be evaluated/.
        -> a         -- ^ Compared to 3rd parameter.
        -> a         -- ^ Compared to 2nd parameter.
        -> Ordering  -- ^ Result of the comparison.

-- | Policy type for a 'MinHeap'.
data MinPolicy

instance (Ord a) => HeapPolicy MinPolicy a where
    heapCompare = const compare

-- | Policy type for a 'MaxHeap'.
data MaxPolicy

instance (Ord a) => HeapPolicy MaxPolicy a where
    heapCompare = const (flip compare)

-- | Policy type for a @(priority, value)@ 'MinPrioHeap'.
data FstMinPolicy

instance (Ord priority) => HeapPolicy FstMinPolicy (priority, value) where
    heapCompare = const (comparing fst)

-- | Policy type for a @(priority, value)@ 'MaxPrioHeap'.
data FstMaxPolicy

instance (Ord priority) => HeapPolicy FstMaxPolicy (priority, value) where
    heapCompare = const (flip (comparing fst))

-- | /O(1)/. Is the 'Heap' empty?
null :: Heap p a -> Bool
null Empty = True
null _     = False

-- | /O(1)/. Is the 'Heap' empty?
isEmpty :: Heap p a -> Bool
isEmpty = null

-- | /O(1)/. Calculate the rank of a 'Heap'.
rank :: Heap p a -> Int
rank Empty            = 0
rank (Tree r _ _ _ _) = r

-- | /O(1)/. The number of elements in the 'Heap'.
size :: Heap p a -> Int
size Empty            = 0
size (Tree _ s _ _ _) = s

-- | This function is 'undefined' and just used as a type-helper to determine
-- the first parameter of 'heapCompare'.
policy :: Heap p a -> p
policy = error $ __FILE__ ++ ": HeapPolicy was evaluated"

-- | /O(log n)/ for the tail, /O(1)/ for the head. Find the minimum (depending
-- on the 'HeapPolicy') and delete it from the 'Heap' (i. e. find head and tail
-- of a heap) if it is not empty. Otherwise, 'Nothing' is returned.
view :: (HeapPolicy p a) => Heap p a -> Maybe (a, Heap p a)
view Empty            = Nothing
view (Tree _ _ x l r) = Just (x, union l r)
{-# INLINE view #-}

-- | /O(1)/. Returns the first item of the 'Heap', according to its 'HeapPolicy'.
--
-- /Warning:/ @'unsafeHead' 'empty' = 'undefined'@, consider using 'view'.
unsafeHead :: (HeapPolicy p a) => Heap p a -> a
unsafeHead = fst . unsafeUncons

-- | /O(log n)/. Returns the 'Heap' with the head removed.
--
-- /Warning:/ @'unsafeTail' 'empty' = 'undefined'@, consider using 'view'.
unsafeTail :: (HeapPolicy p a) => Heap p a -> Heap p a
unsafeTail = snd . unsafeUncons

-- | /O(log n)/ for the tail, /O(1)/ for the head. Head and tail of a 'Heap'.
--
-- /Warning:/ @'unsafeUncons' 'empty' = 'undefined'@, consider using 'view'.
unsafeUncons :: (HeapPolicy p a) => Heap p a -> (a, Heap p a)
unsafeUncons heap = maybe (error (__FILE__ ++ ": empty heap in unsafeUncons")) id (view heap)

-- | /O(1)/. Constructs an empty 'Heap'.
empty :: Heap p a
empty = Empty

-- | /O(1)/. Create a singleton 'Heap'.
singleton :: a -> Heap p a
singleton x = Tree 1 1 x empty empty

-- | /O(log n)/. Insert an element in the 'Heap'.
insert :: (HeapPolicy p a) => a -> Heap p a -> Heap p a
insert x h = union h (singleton x)

-- | /O(1)/. Insert an element into the 'Heap' that is smaller than all elements
-- currently in the 'Heap' (according to the 'HeapPolicy'), i. e. an element
-- that will be the new head of the 'Heap'.
--
-- /The precondition is not checked/.
uncheckedInsertMin :: (HeapPolicy p a) => a -> Heap p a -> Heap p a
uncheckedInsertMin h hs = assert
    (maybe True (\(h', _) -> GT /= heapCompare (policy hs) h h') (view hs))
    (Tree 1 (1 + size hs) h hs empty)

-- | Take the lowest @n@ elements in ascending order of the 'Heap' (according
-- to the 'HeapPolicy').
take :: (HeapPolicy p a) => Int -> Heap p a -> [a]
take n = fst . (splitAt n)

-- | Remove the lowest (according to the 'HeapPolicy') @n@ elements
-- from the 'Heap'.
drop :: (HeapPolicy p a) => Int -> Heap p a -> Heap p a
drop n = snd . (splitAt n)

-- | @'splitAt' n h@ returns an ascending list of the lowest @n@ elements of @h@
-- (according to its 'HeapPolicy') and @h@, with those elements removed.
splitAt :: (HeapPolicy p a) => Int -> Heap p a -> ([a], Heap p a)
splitAt n heap
    | n > 0     = case view heap of
        Nothing      -> ([], empty)
        Just (h, hs) -> let (xs, heap') = splitAt (n-1) hs in (h:xs, heap')
    | otherwise = ([], heap)

-- | @'takeWhile' p h@ lists the longest prefix of elements in ascending order
-- (according to its 'HeapPolicy') of @h@ that satisfy @p@.
takeWhile :: (HeapPolicy p a) => (a -> Bool) -> Heap p a -> [a]
takeWhile p = fst . (span p)

-- | @'dropWhile' p h@ removes the longest prefix of elements from @h@ that
-- satisfy @p@.
dropWhile :: (HeapPolicy p a) => (a -> Bool) -> Heap p a -> Heap p a
dropWhile p = snd . (span p)

-- | @'span' p h@ returns the longest prefix of elements in ascending order
-- (according to its 'HeapPolicy') of @h@ that satisfy @p@ and @h@, with those
-- elements removed.
span :: (HeapPolicy p a) => (a -> Bool) -> Heap p a -> ([a], Heap p a)
span p heap = case view heap of
    Nothing      -> ([], empty)
    Just (h, hs) -> if p h
        then let (xs, heap') = span p hs in (h:xs, heap')
        else ([], heap)

-- | @'break' p h@ returns the longest prefix of elements in ascending order
-- (according to its 'HeapPolicy') of @h@ that do /not/ satisfy @p@ and @h@,
-- with those elements removed.
break :: (HeapPolicy p a) => (a -> Bool) -> Heap p a -> ([a], Heap p a)
break p = span (not . p)

-- | /O(log max(n, m))/. The union of two 'Heap's.
union :: (HeapPolicy p a) => Heap p a -> Heap p a -> Heap p a
union h Empty = h
union Empty h = h
union heap1@(Tree _ _ x l1 r1) heap2@(Tree _ _ y l2 r2) =
    if LT == heapCompare (policy heap1) x y
        then makeT x l1 (union r1 heap2) -- keep smallest number on top and merge the other
        else makeT y l2 (union r2 heap1) -- heap into the right branch, it's shorter

-- | Combines a value @x@ and two 'Heap's to one 'Heap'. Therefore, @x@ has to
-- be less or equal the minima (depending on the 'HeapPolicy') of both 'Heap'
-- parameters.
--
-- /The precondition is not checked/.
makeT :: a -> Heap p a -> Heap p a -> Heap p a
makeT x a b = let
    ra = rank a
    rb = rank b
    s  = size a + size b + 1
    in if ra > rb
        then Tree (rb + 1) s x a b
        else Tree (ra + 1) s x b a
{-# INLINE makeT #-}

-- | Builds the union over all given 'Heap's.
unions :: (HeapPolicy p a) => [Heap p a] -> Heap p a
unions heaps = case tournamentFold' heaps of
    []  -> empty
    [h] -> h
    hs  -> unions hs
    where
    tournamentFold' :: (Monoid m) => [m] -> [m]
    tournamentFold' (x1:x2:xs) = (: tournamentFold' xs) $! mappend x1 x2
    tournamentFold' xs         = xs
    {-# INLINE tournamentFold' #-}

-- | Removes all elements from a given 'Heap' that do not fulfil the predicate.
filter :: (HeapPolicy p a) => (a -> Bool) -> Heap p a -> Heap p a
filter p = fst . (partition p)

-- | Partition the 'Heap' into two. @'partition' p h = (h1, h2)@: All elements
-- in @h1@ fulfil the predicate @p@, those in @h2@ don't. @'union' h1 h2 = h@.
partition :: (HeapPolicy p a) => (a -> Bool) -> Heap p a -> (Heap p a, Heap p a)
partition _ Empty = (empty, empty)
partition p (Tree _ _ x l r)
    | p x       = (makeT x l1 r1, union l2 r2)
    | otherwise = (union l1 r1, makeT x l2 r2)
    where
    (l1, l2) = partition p l
    (r1, r2) = partition p r

-- | /O(n log n)/. Builds a 'Heap' from the given elements. Assuming you have a
-- sorted 'Foldalbe', you may want to use 'fromDescFoldable' or 'fromAscFoldable',
-- they are faster than this function. Note that this function also works on
-- lists, so no separate @fromList@ function is provided.
fromFoldable :: (HeapPolicy p a, Foldable f) => f a -> Heap p a
fromFoldable xs = let
    list = Foldable.toList xs
    heap = fromDescFoldable $ sortBy (flip (heapCompare (policy heap))) list
    in heap
{-# SPECIALISE fromFoldable :: (HeapPolicy p a) => [a] -> Heap p a #-}

-- | /O(n)/. Lists elements of the 'Heap' in no specific order.
toList :: Heap p a -> [a]
toList Empty            = []
toList (Tree _ _ x l r) = x : if size r < size l
    then toList r ++ toList l
    else toList l ++ toList r

-- | /O(n)/. Creates a 'Heap' from an ascending 'Foldable' implementation. Note
-- that it has to be ascending corresponding to the 'HeapPolicy', not to its
-- 'Ord' instance declaration (if there is one). This function is faster than
-- 'fromFoldable' but not as fast as 'fromDescFoldable'. Note that this function
-- also works on lists, so no separate @fromAscList@ function is provided.
--
-- /The precondition is not checked/.
fromAscFoldable :: (HeapPolicy p a, Foldable f) => f a -> Heap p a
fromAscFoldable = fromDescFoldable . reverse . Foldable.toList
{-# SPECIALISE fromAscFoldable :: (HeapPolicy p a) => [a] -> Heap p a #-}

-- | /O(n)/. Lists elements of the 'Heap' in ascending order (corresponding to
-- the 'HeapPolicy').
toAscList :: (HeapPolicy p a) => Heap p a -> [a]
toAscList = takeWhile (const True)

-- | /O(n)/. Create a 'Heap' from a descending 'Foldable' implementation. Note
-- that it has to be descending corresponding to the 'HeapPolicy', not to its
-- 'Ord' instance declaration (if there is one). This function is provided,
-- because it is faster than 'fromFoldable' and 'fromAscFoldable'. Note that
-- this function also works on lists, so no separate @fromDescList@ function is
-- provided.
--
-- /The precondition is not checked/.
fromDescFoldable :: (HeapPolicy p a, Foldable f) => f a -> Heap p a
fromDescFoldable = foldl' (flip uncheckedInsertMin) empty
{-# SPECIALISE fromDescFoldable :: (HeapPolicy p a) => [a] -> Heap p a #-}

-- | /O(n)/. Lists the elements on the 'Heap' in descending order (corresponding
-- to the 'HeapPolicy'). Note that this function is not especially efficient (it
-- is implemented as @'reverse' . 'toAscList'@), it is just provided as a
-- counterpart of the efficient 'fromDescFoldable' function.
toDescList :: (HeapPolicy p a) => Heap p a -> [a]
toDescList = reverse . toAscList
