{-# LANGUAGE CPP, DeriveDataTypeable, ScopedTypeVariables #-}

-- | This module provides a simple leftist-heap implementation based on Chris
-- Okasaki's book \"Purely Functional Data Structures\", Cambridge University
-- Press, 1998, chapter 3.1.
--
-- A @'Heap' prio val@ associates a priority @prio@ to a value @val@. A
-- priority-value pair with minimum priority will always be the head of the
-- 'Heap', so this module provides minimum priority heaps. Note that the value
-- associated to the priority has no influence on the ordering of elements, only
-- the priority does.
--
-- The "Data.Heap" module provides a much more convenient 'Heap' access, so you
-- probably want to use it instead of using this module directly.
module Data.Heap.Internal
    ( -- * A basic heap type
#ifdef __DEBUG__
      Heap(..)
#else
      Heap
#endif
      -- * Query
    , isEmpty, rank, size
      -- * Construction
    , empty, singleton, union, unions
      -- * Deconstruction
    , view
      -- * Filter
    , partition
      -- * Subranges
    , splitAt, span
      -- * Conversion
    , fromFoldable, fromDescFoldable
    , toList, toAscList
    ) where

import Control.Exception ( assert )
import Data.Binary ( Binary(..) )
import Data.Foldable ( Foldable(..), foldl' )
import qualified Data.Foldable as Foldable ( toList )
import Data.List ( sortBy )
import Data.Monoid ( Monoid(..) )
import Data.Ord ( comparing )
import Data.Typeable ( Typeable )
import Prelude hiding ( foldl, span, splitAt )
import Text.Read ( Lexeme(..), Read(..), lexP, parens, prec, readListPrecDefault )

-- | The basic 'Heap' type. It stores priority-value pairs @(prio, val)@ and
-- always keeps the pair with minimal priority on top. The value associated to
-- the priority does not have any influence on the ordering of elements.
data Heap prio val
    = Empty  -- ^ An empty 'Heap'.
    | Tree { _rank     :: {-# UNPACK #-} !Int -- ^ Rank of the leftist heap.
           , _size     :: {-# UNPACK #-} !Int -- ^ Number of elements in the heap.
           , _priority :: !prio               -- ^ Priority of the entry.
           , _value    :: val                 -- ^ Value of the entry.
           , _left     :: !(Heap prio val)    -- ^ Left subtree.
           , _right    :: !(Heap prio val)    -- ^ Right subtree.
           } -- ^ A tree node of a non-empty 'Heap'.
    deriving ( Typeable )

instance (Read prio, Read val, Ord prio) => Read (Heap prio val) where
    readPrec     = parens $ prec 10 $ do
      Ident "fromList" <- lexP
      xs               <- readPrec
      return ((fromFoldable :: [(prio, val)] -> Heap prio val) xs)
    readListPrec = readListPrecDefault

instance (Show prio, Show val) => Show (Heap prio val) where
    show = ("fromList " ++) . show . toList

instance (Ord prio, Ord val) => Eq (Heap prio val) where
    heap1 == heap2 = EQ == compare heap1 heap2

instance (Ord prio, Ord val) => Ord (Heap prio val) where
    compare = comparing toAscList

instance (Binary prio, Binary val) => Binary (Heap prio val) where
    put Empty                = put False
    put (Tree r s p v hl hr) = put True >> put (r, s, p, v, hl, hr)
    get = do
      node <- get
      if node then do (r, s, p, v, hl, hr) <- get
                      return $ Tree r s p v hl hr
              else return Empty

instance (Ord prio) => Monoid (Heap prio val) where
    mempty  = empty
    mappend = union
    mconcat = unions

instance Functor (Heap prio) where
    fmap _ Empty = Empty
    fmap f heap  = heap { _value = f (_value heap)
                        , _left  = fmap f (_left heap)
                        , _right = fmap f (_right heap)
                        }

instance (Ord prio) => Foldable (Heap prio) where
    foldMap f = foldMap f . fmap snd . toAscList
    foldr f z = foldl (flip f) z . fmap snd . reverse . toAscList
    foldl f z = foldl f z . fmap snd . toAscList

-- | /O(1)/. Is the 'Heap' empty?
isEmpty :: Heap prio val -> Bool
isEmpty Empty = True
isEmpty _     = False
{-# INLINE isEmpty #-}

-- | /O(1)/. Find the rank of a 'Heap', which is the length of its right spine.
rank :: Heap prio val -> Int
rank Empty = 0
rank heap  = _rank heap
{-# INLINE rank #-}

-- | /O(1)/. The total number of elements in the 'Heap'.
size :: Heap prio val -> Int
size Empty = 0
size heap  = _size heap
{-# INLINE size #-}

-- | /O(1)/. Constructs an empty 'Heap'.
empty :: Heap prio val
empty = Empty
{-# INLINE empty #-}

-- | /O(1)/. Create a singleton 'Heap'.
singleton :: prio -> val -> Heap prio val
singleton p v = Tree { _rank     = 1
                     , _size     = 1
                     , _priority = p
                     , _value    = v
                     , _left     = empty
                     , _right    = empty
                     }
{-# INLINE singleton #-}

-- | /O(1)/. Insert an priority-value pair into the 'Heap', whose /priority is
-- less or equal/ to all other priorities on the 'Heap', i. e. a pair that is a
-- valid head of the 'Heap'.
--
-- /The precondition is not checked/.
uncheckedCons :: (Ord prio) => prio -> val -> Heap prio val -> Heap prio val
uncheckedCons p v heap = assert (maybe True (\(p', _, _) -> p <= p') (view heap))
                         Tree { _rank     = 1
                              , _size     = 1 + size heap
                              , _priority = p
                              , _value    = v
                              , _left     = heap
                              , _right    = empty
                              }
{-# INLINE uncheckedCons #-}

-- | /O(log max(n, m))/. The union of two 'Heap's.
union :: (Ord prio) => Heap prio val -> Heap prio val -> Heap prio val
union heap  Empty = heap
union Empty heap  = heap
union heap1 heap2 = let p1 = _priority heap1
                        p2 = _priority heap2
                    in if p1 < p2
    then makeT p1 (_value heap1) (_left heap1) (union (_right heap1) heap2)
    else makeT p2 (_value heap2) (_left heap2) (union (_right heap2) heap1)

-- | Builds a 'Heap' from a priority, a value and two more 'Heap's. Therefore,
-- the /priority has to be less or equal/ than all priorities in both 'Heap'
-- parameters.
--
-- /The precondition is not checked/.
makeT :: (Ord prio) => prio -> val -> Heap prio val -> Heap prio val -> Heap prio val
makeT p v a b = let ra = rank a
                    rb = rank b
                    s  = size a + size b + 1
                in assert (checkPrio a && checkPrio b)
                       $ if ra > rb then Tree (rb + 1) s p v a b
                                    else Tree (ra + 1) s p v b a
    where checkPrio = maybe True (\(p', _, _) -> p <= p') . view
{-# INLINE makeT #-}

-- | Builds the union over all given 'Heap's.
unions :: (Ord prio) => [Heap prio val] -> Heap prio val
unions heaps = case tournamentFold' heaps of
    []  -> empty
    [h] -> h
    hs  -> unions hs
    where
    tournamentFold' :: (Monoid m) => [m] -> [m]
    tournamentFold' (x1:x2:xs) = (: tournamentFold' xs) $! mappend x1 x2
    tournamentFold' xs         = xs
    {-# INLINE tournamentFold' #-}

-- | /O(log n)/ for the tail, /O(1)/ for the head. Find the priority-value pair
-- with minimal priority and delete it from the 'Heap' (i. e. find head and tail
-- of the heap) if it is not empty. Otherwise, 'Nothing' is returned.
view :: (Ord prio) => Heap prio val -> Maybe (prio, val, Heap prio val)
view Empty = Nothing
view heap  = Just (_priority heap, _value heap, union (_left heap) (_right heap))
{-# INLINE view #-}

-- | Partition the 'Heap' into two. @'partition' p h = (h1, h2)@: All
-- priority-value pairs in @h1@ fulfil the predicate @p@, those in @h2@ don't.
-- @'union' h1 h2 = h@.
partition :: (Ord prio) => (prio -> val -> Bool) -> Heap prio val
          -> (Heap prio val, Heap prio val)
partition _ Empty  = (empty, empty)
partition f heap
    | f p v     = (makeT p v l1 r1, union l2 r2)
    | otherwise = (union l1 r1, makeT p v l2 r2)
    where p        = _priority heap
          v        = _value heap
          (l1, l2) = partition f (_left heap)
          (r1, r2) = partition f (_right heap)
{-# INLINE partition #-}

-- | @'splitAt' n h@ returns a list of the lowest @n@ priority-value pairs of @h@,
-- in  ascending order of priority, and @h@, with those elements removed.
splitAt :: (Ord prio) => Int -> Heap prio val -> ([(prio, val)], Heap prio val)
splitAt n heap
    | n > 0     = case view heap of
                    Nothing         -> ([], empty)
                    Just (p, v, hs) -> let (xs, heap') = splitAt (n-1) hs
                                       in ((p, v) : xs, heap')
    | otherwise = ([], heap)
{-# INLINE splitAt #-}

-- | @'span' p h@ returns the longest prefix of priority-value pairs of @h@, in
-- ascending order of priority, that satisfy @p@ and @h@, with those elements
-- removed.
span :: (Ord prio) => (prio -> val -> Bool) -> Heap prio val
     -> ([(prio, val)], Heap prio val)
span f heap
    = case view heap of
        Nothing         -> ([], empty)
        Just (p, v, hs) ->
            if f p v then let (xs, heap') = span f hs in ((p, v):xs, heap')
                     else ([], heap)
{-# INLINE span #-}

-- | /O(n log n)/. Builds a 'Heap' from the given priority-value pairs. Assuming
-- you have a sorted 'Foldable', you probably want to use 'fromDescFoldable' or
-- 'fromAscFoldable', they are faster than this function.
fromFoldable :: (Foldable f, Ord prio) => f (prio, val) -> Heap prio val
fromFoldable xs = let
    list = Foldable.toList xs
    heap = fromDescFoldable $ sortBy (flip (comparing fst)) list
    in heap
{-# INLINE fromFoldable #-}
{-# SPECIALISE fromFoldable :: (Ord prio) => [(prio, val)] -> Heap prio val #-}

-- | /O(n)/. Create a 'Heap' from a 'Foldable' providing its priority-value pairs
-- in descending order of priority. Prefer this function over 'fromFoldable' and
-- 'fromAscFoldable', as it is faster.
--
-- /The precondition is not checked/.
fromDescFoldable :: (Foldable f, Ord prio) => f (prio, val) -> Heap prio val
fromDescFoldable = foldl' (\h (p, v) -> uncheckedCons p v h) empty
{-# INLINE fromDescFoldable #-}
{-# SPECIALISE fromDescFoldable :: (Ord prio) => [(prio, val)] -> Heap prio val #-}

-- | /O(n log n)/. Lists all priority-value pairs of the 'Heap' in no specific
-- order.
toList :: Heap prio val -> [(prio, val)]
toList Empty = []
toList heap  = let left  = _left heap
                   right = _right heap
               in (_priority heap, _value heap) : if (size right) < (size left)
                                                  then toList right ++ toList left
                                                  else toList left  ++ toList right
{-# INLINE toList #-}

-- | /O(n log n)/. Lists priority-value pairs of the 'Heap' in ascending order of
-- priority.
toAscList :: (Ord prio) => Heap prio val -> [(prio, val)]
toAscList = fst . span (\_ _ -> True)
{-# INLINE toAscList #-}