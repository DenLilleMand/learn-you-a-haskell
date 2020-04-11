doubleMe x = x + x
doubleUs x y = x*2 + y*2
doubleSmallNumber x = if x > 100
                        then x
                        else x*2
conanO'Brien = "It's a-me, Canan O'Brien!"

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

factorial :: Int -> Int
factorial n = product [1..n]

something :: (Integral a) => a -> a
something 7 = 8
something 10 = 12
something x = 14

factorial' :: (Integral a) => a -> a
factorial' 0 = 1
factorial' n = n * factorial' (n - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b = (fst a + fst b, snd a + snd b)

-- Nicer way to add two vectors using pattern matching
addVectors' :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors' (x, y) (x', y') = (x+x', y+y')

sum' :: [Int] -> Int
sum' [] = 0
sum' (x : xs) = x + sum' xs

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x

intact :: [Int] -> Int
intact [] = 0
intact xs@(x:y:z:ys) = x + y + z + intact ys

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal pfffft, i bet you're ugly"
    | bmi <= fat    = "You're fat!"
    | otherwise     = "You're a whale congratulations!"
    where bmi = weight / height ^ 2
          skinny = 18.5
          normal = 25.0
          fat = 30.0


-- Some mix of guards and patterns
herpderp :: [Int] -> Int
herpderp (x:_) | x > 50 = 60
herpderp (x:_) = x

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b = a
    | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a > b = GT
    | a < b = LT
    | otherwise = EQ

-- i guess the little interesting part here is that the
-- where clause gets evaluated recursively all the way down
-- until it hits the err where xs becomes empty, the error at
-- that point does not crash the whole thing, it just ignores it and
-- goes to the next pattern
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x:replicate' (n-1) x


take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : (take' (n-1) xs)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : (zip' xs ys)

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' x' (x:xs)
    | x' == x = True
    | otherwise = x' `elem'` xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = let smallerList = quicksort [a | a <- xs, a <= x ]
                       biggerList = quicksort [a | a <- xs, a > x] in
                       smallerList ++ [x] ++ biggerList


