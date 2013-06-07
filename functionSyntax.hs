--recursive factorial function
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)

--add vectors together using tuples without pattern matching
addVectors :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVectors a b = (fst a + fst b, snd a + snd b)

--add vectors using pattern matching
addVectors' :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVectors' (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

--find slope of function
slope :: (Fractional a) => (a,a) -> (a,a) -> a
slope (x1, y1) (x2, y2) = ( (y2 - y1) / (x2 - x1) )

--first element of triple tuple
first :: (a, b, c) -> a
first (x, _, _) = x

--second element of triple tuple
second :: (a, b, c) -> b
second (_, y, _) = y

--third element of triple tuple
third :: (a, b, c) -> c
third (_, _, z) = z

--head function using pattern matching
head' :: [a] -> a
head' [] = error "Can't call head on empty list!"
head' (x:_) = x

--tells user first elements of list
tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long, the first two are: " ++ show x ++ " and " ++ show y

--recursive length function
length' :: (Num b) => [a] -> b
legnth' [] = 0
length' (_:xs) = 1 + length' xs

--pattern matching
captial :: String -> String
captial "" = "Empty string dummy"
captial all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

--using guards in a function
bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
	| bmi <= 18.5 = "You're underweight brudda"
	| bmi <= 25.0 = "You're so bland and normal brudda"
	| bmi <= 30.0 = "Wow brudda don't eat me!"
	| otherwise = "You're a whale, congrats you piece of shit"

--because of guards a max function is great
max' :: (Ord a) => a -> a -> a
max' a b
	| a > b		=a
	| otherwise	=b

--compate function to practice guards
myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
	| a > b		= GT
	| a == b	= EQ
	| otherwise	= LT

--bmi function using WHERE keyword
bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' weight height
	| bmi <= 18.5 = "You're underweight brudda"
	| bmi <= 25.0 = "You're so bland and normal brudda"
    | bmi <= 30.0 = "Wow brudda don't eat me!"
	| otherwise	  = "You're a whale you whore"
	where bmi = weight / height ^ 2

--initials function using where and pattern matching
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
	where (f:_) = firstname
	      (l:_) = lastname

--example of let bindings
-- **Note: let is similar to where, but is an expression itself
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
	let sideArea = 2 * pi * r * h
	    topArea = pi * r ^2
	in sideArea + 2 * topArea 

