safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast (x:xs) = if null xs
                  then Just x
                  else safeLast xs

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit (x:[]) = Nothing
safeInit xs = let (y:ys) = reverse xs
              in Just (reverse ys)

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith f xs = let fs = filter f xs
                     nfs = filter (\x -> not (f x)) xs
                 in [nfs, fs]

transposeText :: String -> String
transposeText input = let (l:r:[]) = lines input
                      in concat $ zipWith (\a b -> a:b:'\n':[]) l r