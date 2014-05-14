import Data.Char

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

asInt :: String -> Int
asInt [] = 0
asInt (c:cs) = if c=='-'
               then foldl (\a c -> a*10 - digitToInt c) 0 cs
               else foldl (\a c -> a*10 + digitToInt c) 0 (c:cs)
               
type ErrorMessage = String
asInt_either :: String -> Either ErrorMessage Int
asInt_either [] = Left "Contains no digit"
asInt_either ('-':cs) = case asInt_either cs of
                          Left err -> Left err
                          Right val -> Right (-val)
asInt_either cs = foldl step (Right 0) cs
  where step (Right a) c | isDigit c = Right (a*10 + digitToInt c)
                         | otherwise = Left ([c] ++ " is not a digit!")
        step (Left err) _ = Left err
        
groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' f xl = step [] xl
  where step acc [] = reverse acc
        step acc (x:xs) = step (([x] ++ (takeWhile (\a -> f x a) xs)):acc)
                            (dropWhile (\a -> f x a) xs)
                          
                          
                          
                          
                          
                          
                          
                          
                          
                          
                          