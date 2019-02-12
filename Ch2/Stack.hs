{-# LANGUAGE InstanceSigs #-}

module Stack (Stack(..), List(..)) where
-- import Prelude hiding (head, tail)

class Stack s where
  empty :: s a 
  isEmpty :: s a -> Bool
  cons :: a -> s a -> s a
  head :: s a -> a
  tail :: s a -> s a

data List a = NIL | CONS a (List a)

instance Stack List where

  empty :: List a 
  empty = NIL

  isEmpty :: List a -> Bool
  isEmpty NIL = True
  isEmpty (CONS _ _) = False

  cons :: a -> List a -> List a
  cons = CONS

  head :: List a -> a
  head NIL = error "Empty"
  head (CONS x _) = x

  tail :: List a -> List a
  tail NIL = error "Empty"
  tail (CONS _ xs) = xs
