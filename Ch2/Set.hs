{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, InstanceSigs #-}

module Ch2.Set
( Set(..)
, UnbalancedSet(..)  
) where


class Set s a where
  empty :: s a
  insert :: a -> s a -> s a
  member :: a -> s a -> Bool


data UnbalancedSet a = E | T (UnbalancedSet a) a (UnbalancedSet a)
                    deriving (Show, Eq, Ord)

-- This UnbalancedSet is lazied, use bang! to make it eager.

instance (Ord a) => Set UnbalancedSet a where
  empty :: UnbalancedSet a
  empty = E

  insert :: a -> UnbalancedSet a -> UnbalancedSet a
  insert x E = T E x E
  insert x s@(T a y b) 
    | x < y = T (insert x a) y b 
    | x > y = T a y (insert x b)
    | otherwise = s

  member :: a -> UnbalancedSet a -> Bool
  member _ E = False
  member x (T a y b)
    | x < y = member x a
    | x > y = member x b 
    | otherwise = True





