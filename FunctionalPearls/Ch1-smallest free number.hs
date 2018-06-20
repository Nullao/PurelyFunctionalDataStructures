module Ch1 where
import Data.Array.IArray
import Data.Array.ST
import Data.List

-- Array-based solution

minfree :: [Int] -> Int
minfree xs = head ([0..] \\ xs)

{-
(\\) :: Eq a => [a] -> [a] -> [a]
us \\ vs = filter (flip notElem vs) us
-}

search :: Array Int Bool -> Int
search = length . takeWhile id . elems

checklist :: [Int] -> Array Int Bool 
checklist xs = accumArray (||) False (0, n) 
               (zip (filter (<= n) xs) (repeat True))
                    where n = length xs

checklist' :: [Int] -> Array Int Bool 
checklist' xs = runSTArray (do
                     a <- newArray (0, n) False;
                     _ <- sequence_ [writeArray a x True | x <- xs, x <= n];
                     return a)
                where n = length xs

countlist :: [Int] -> Array Int Int 
countlist xs = accumArray (+) 0 (0, n) (zip xs (repeat 1))
                    where n = length xs

sort :: [Int] -> [Int]
sort xs = concat [replicate k x | (x, k) <- assocs(countlist xs)]


-- Divide & Conquer

minfrom :: Int -> (Int, [Int]) -> Int
minfrom a (n, xs) | n == 0 = a 
                  | m == b - a = minfrom b (n - m, vs)
                  | otherwise = minfrom a (m, us)
                    where (us, vs) = partition (< b) xs
                          b        = a + 1 + n `div` 2
                          m        = length us

minfree' :: [Int] -> Int
minfree' xs = minfrom 0 (length xs, xs)
