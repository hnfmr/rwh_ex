length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

mean :: (Fractional a) => [a] -> a
mean [] = 0
mean xs = sum xs / fromIntegral (length xs)

reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

toPalindrome xs = xs ++ (reverse' xs)

isPalindrome xs = xs == (reverse' xs)

intersperse' :: a -> [[a]] -> [a]
intersperse' _ [] = []
intersperse' s (x:xs) = (x ++ [s]) ++ (intersperse' s xs)

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

height :: Tree a -> Int
height Empty = 0
height (Node a b c) = 1 + max (height b) (height c)

data Point = Point { x :: Double,
                     y :: Double }
             deriving (Show)

xdiff :: Point -> Point -> Double
xdiff (Point x0 y0) (Point x1 y1) = abs (x0 - x1)

ydiff :: Point -> Point -> Double
ydiff (Point x0 y0) (Point x1 y1) = abs (y0 - y1)

data Direction = LeftT | RightT | StraightT deriving (Show)

data Line = Line { a :: Double,
                   b :: Double,
                   c :: Double  }
            deriving (Show)

data PointDirection = LeftL | RightL | SameL

gradient :: Point -> Point -> Double
gradient (Point x0 y0) (Point x1 y1) = (y1-y0)/(x1-x0)

intercept :: Point -> Point -> Double
intercept a b =
  let (Point x0 y0) = a
  in x0 - (gradient a b)*x0

gradient' :: Line -> Double
gradient' (Line a b c) = (-a)/b

intercept' :: Line -> Double
intercept' (Line _ b c) = (-c)/b

line :: Point -> Point -> Line
line (Point a1 b1) (Point a2 b2) = let a = b1 - b2
                                       b = a2 - a1
                                       c = a1*b2 - a2*b1
                                   in (Line a b c)

pointDirection :: Point -> Line -> PointDirection
pointDirection (Point x0 y0) (Line a b c)
  | (a*x0+b*y0+c < 0) = LeftL
  | (a*x0+b*y0+c > 0) = RightL
  | otherwise = SameL

distance :: Point -> Point -> Double
distance (Point x0 y0) (Point x1 y1) =
  let ydiff = abs (y1 - y0)
      xdiff = abs (x1 - x0)
  in sqrt $ (ydiff^2 + xdiff^2)

distanceToLine :: Point -> Line -> Double
distanceToLine (Point x y) (Line a b c) = let numerator = abs (a*x + b*y + c)
                                              denumerator = let operand = a^2 + b^2
                                                            in sqrt operand
                                          in
                                          let distance' = numerator / denumerator
                                          in abs distance'


radianToDegree :: Double -> Double
radianToDegree r = 360 * r / (2*pi)

abAngle :: Point -> Point -> Double
abAngle a b = atan $ ydiff a b / xdiff a b

turn :: Point -> Point -> Point -> Direction
turn a b c
  | prod > 0 = LeftT
  | prod < 0 = RightT
  | otherwise = StraightT
  where prod = (gradient a b)/(gradient a c)

slidingDirection :: [Point] -> [Direction]
slidingDirection [] = []
slidingDirection (a:[]) = []
slidingDirection (a:b:[]) = []
slidingDirection (a:b:c:ps) = turn a b c : slidingDirection (b:c:ps)


{-
angle :: Point -> Point -> Point -> Double
angle a b c = let abDist = distance a b
                  bcDist = distance b c
              in
              let angleAB = radianToDegree (asin (dAB/abDist))
                  angleBC = radianToDegree (asin (dBC/bcDist))
              in 180.0 - angleAB - angleBC
              where dAB = distanceToLine b (line b c)
                    dBC = distanceToLine b (line a b)
-}           


--angleDirection :: Point -> Point -> Direction
--angleDirection (Point x1 y1) (Point x2 y2)
--    | 
