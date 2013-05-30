-- function with type declaration
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z'] ]

--function that takes three integers and adds them together
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

--Test using class constraints
isEqual :: Eq x => x -> x -> Bool
isEqual a b = if a == b 
	then True 
	else False

--floating class constraint
floatMe :: Floating a => a -> a -> a -> a
floatMe x y z = x + 2*y + z
