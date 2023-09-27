-- Problem 1
pentanacci :: Int -> Int
pentanacci n | n < 5     = 0
             | n == 5    = 1
             | otherwise = pentanacci (n - 1) + 
                           pentanacci (n - 2) + 
                           pentanacci (n - 3) + 
                           pentanacci (n - 4) + 
                           pentanacci (n - 5)

-- Problem 2
sort :: Ord a => [a] -> [a]
sort []     = []
sort (x:xs) = sort smaller ++ [x] ++ sort larger
            where
              smaller = [a | a <- xs, x >= a]
              larger  = [a | a <- xs, x < a]

wave :: [Int] -> [Int]
wave []  = []
wave [x] = [x]
wave xs  =  [largestElement, smallestElement] ++ wave middleList
         where
           sortedList      = sort xs
           smallestElement = head sortedList
           largestElement  = last sortedList
           middleList      = (tail . init) sortedList

-- Problem 3
isTwinPaired :: [Int] -> Bool
isTwinPaired [] = True
isTwinPaired xs = isSorted evenElements && isSorted oddElements
               where
                 evenElements = [x | x <- xs, even x]
                 oddElements  = [x | x <- xs, odd x]
                 isSorted ys  = and [x <= y | (x,y) <- zip ys (tail ys)]

-- Problem 4
isMatrian :: [Int] -> Bool
isMatrian []  = False
isMatrian [x] = x == 1
isMatrian xs = numberOfOnes > numberOfTwos && not hasAdjacentElems
    where
        numberOfOnes      = length [x | x <- xs, x == 1]
        numberOfTwos      = length [x | x <- xs, x == 2]
        hasAdjacentElems  = or [x == y | (x,y) <- zip xs (tail xs)]

-- Problem 5
toDigits :: Integer -> [Integer]
toDigits n | n <= 0    = []
           | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n | n <= 0    = []
              | otherwise = (n `mod` 10) : toDigitsRev (n `div` 10)

-- Problem 6
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []       = []
doubleEveryOther [x]      = [x]
doubleEveryOther (x:y:xs) = if even (length xs) 
                            then x*2 : y : doubleEveryOther xs
                            else x : y*2 : doubleEveryOther xs

-- Problem 7
sumDigits :: [Integer] -> Integer
sumDigits = foldr (\x y -> x `div` 10 + x `mod` 10 + y) 0

-- Problem 8
validate :: Integer -> Bool
validate n | n < 0     = False
           | otherwise = (sumDigits . doubleEveryOther . toDigits) n `mod` 10 == 0

-- Problem 9
sortp :: [a] -> (a -> a -> Bool) -> [a]
sortp [] f     = []
sortp (x:xs) f = sortp smaller f ++ [x] ++ sortp larger f
               where
                 smaller = [a | a <- xs, f a x]
                 larger  = [a | a <- xs, not (f a x)]

-- Problem 10
foldlp :: (b -> Bool) -> (b -> a -> b) -> b -> [a] -> b
foldlp p f v [] = v
foldlp p f v (x:xs) = if p (f v x)
                      then foldlp p f (f v x) xs
                      else v

-- Problem 11
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c | n <= 0    = []
              | n == 1    = [(a,b)]
              | otherwise = hanoi (n - 1) a c b ++ [(a,b)] ++ hanoi (n - 1) c b a
