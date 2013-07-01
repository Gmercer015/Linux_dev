--example function for showing what actually happens when called
multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

--funtion that uses a return function to compare number with 100
compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100 --compare with Hundred now is the same as compare 100, so passing an arg compares

--infix function
divideByTen :: (Floating a) => a -> a
divideByTen = (/10) --10 can be on either side, important is the ()

--infix function example in use
isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z']) --inserts arg into infix expression

--function that take functions as parameters
applyTwice :: (a -> a) -> a -> a --function that returns type a, arg with type a
applyTwice f x = f (f x)

--joins two lists by applying the function passed to them
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = [] --base cases
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys --general case

--takes a function and returns a function that has flipped arguments
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
	where g x y = f y x

--map takes a function and a list, then applies that function to every element in the list
map'' :: (a -> b) -> [a] -> [b]
map'' _ [] = []
map'' f (x:xs) = f x : map f xs

--takes a predicate and a list then returns the list that satisfy the predicate
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = [] --base case
filter' p (x:xs)
	| p x		= x : filter p xs --if value satisfies predicate
	| otherwise = filter p xs	  --else, skip

--quicksort taking advantage of the filter function
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = [] --base case
quicksort (x:xs) =
	let smallerSorted = quicksort (filter (<=x) xs)
	    biggerSorted = quicksort (filter (>x) xs)
	in smallerSorted ++ [x] ++ biggerSorted

--use haskell laziness to find the largest number divisible by 3829 starting at 100000
largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
	where p x = x `mod` 3829 == 0

--collatz function which is damn cool
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
	| even n = n:chain (n `div` 2)
	| odd n = n:chain (n*3 + 1)

--finds long chains
numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
	where isLong xs = length xs > 15

--sum function using fold 
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs --lambda function

--elem function using fold and lambda function
elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

--simple map function using right fold
map' :: (a->b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

--Finds how many elements needed for the sum of the roots of all natural numbers to exceed 1000
sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1
