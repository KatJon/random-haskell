import Data.List
import Data.Ord
import Data.Function

lmss :: Ord a => [a] -> [a]
lmss [] = []
lmss (x:xs) = x: pick (foldr roll [[]] xs)
    where
        roll :: Ord a => a -> [[a]] -> [[a]]
        roll y ts = [ext y t | t <- ts]

        ext y [] = [y]
        ext y t = if head t > y then y:t else t

        pick = maximumBy (compare `on` length) .
            filter (\l -> null l || x < head l)

ex1 = [3,10,4,5,6,7,8]
