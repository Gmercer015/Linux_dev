--Following functions all take advantage of recursion to solve 
--the given problem

--find maximum in a list
maximum' [] = error "cannot take max of empty list"
maximum' [x] = x --base case
maximum' (x:xs)  --general case
	| x > maxTail = x
	| otherwise = maxTail
	where maxTail = maximum' xs

--repeat value x n times and place in list
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
	| n <= 0	= []
	| otherwise = x:replicate' (n-1) x

--take x elements from list n
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _				--make sure the user doesn't want zero elements
	| n <= 0	= []	
take' _ []		= []	--check if list is empty
take' n (x:xs) = x : take' (n-1) xs

--reverse a list
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

--take two lists and zip them together in tuples
zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

--determines if element passed is in list
elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
	| a == x	= True
	| otherwise = a `elem'` xs

--quicksort in haskell is super cheesy
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
	let smallerSorted = quicksort [a | a <- xs, a <= x]
	    biggerSorted = quicksort [a | a <- xs, a > x]
	in smallerSorted ++ [x] ++ biggerSorted
