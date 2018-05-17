{-# LANGUAGE InstanceSigs #-}

module LeftistHeap 
( LeftistHeap(..)
) where

  import Heap

  data LeftistHeap a = E | T Int a (LeftistHeap a) (LeftistHeap a)

  rank :: LeftistHeap a -> Int -- O(1)
  rank E = 0
  rank (T r _ _ _) = r

  makeT :: a -> LeftistHeap a -> LeftistHeap a -> LeftistHeap a  -- O(1)
  makeT x a b = if rank a >= rank b 
                then T (rank b + 1) x a b
                else T (rank a + 1) x b a


  instance Heap LeftistHeap where
    empty     :: Ord a => LeftistHeap a
    empty     = E

    isEmpty   :: Ord a => LeftistHeap a -> Bool -- O(1)
    isEmpty E = True
    isEmpty _ = False

    insert    :: Ord a => a -> LeftistHeap a -> LeftistHeap a -- O(1)
    insert x = merge (T 1 x E E)

    merge     :: Ord a => LeftistHeap a -> LeftistHeap a -> LeftistHeap a -- O(height)
    merge h E = h
    merge E h = h
    merge h1@(T _ x a1 b1) h2@(T _ y a2 b2) = if x <= y 
                                              then makeT x a1 (merge b1 h2)
                                              else makeT y a2 (merge b2 h1)

    findMin   :: Ord a => LeftistHeap a -> a -- O(1)
    findMin E = error "Empty LeftistHeap"
    findMin (T _ x _ _) = x

    deleteMin :: Ord a => LeftistHeap a -> LeftistHeap a -- O(height)
    deleteMin E = error "Empty LeftistHeap"
    deleteMin (T _ _ a b) = merge a b









