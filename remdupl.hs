rdFR :: Eq a => [a] -> [a]
rdFR = foldr go []
    where
        go x [] = [x]
        go x ys = 
            if x == head ys then ys
            else x:ys

rdFL :: Eq a => [a] -> [a]
rdFL = reverse . foldl go []
    where
        go [] x = [x]
        go ys x = 
            if x == head ys then ys
            else x:ys

rdD :: Eq a => [a] -> [a]
rdD xs = go xs Nothing
    where
        go [] _ = []
        go (x:xs) Nothing = x: go xs (Just x)
        go (x:xs) (Just y) =
            if x == y then go xs (Just x)
            else x: go xs (Just x)

ex n = [1..n] >>= replicate 10
