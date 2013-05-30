doubleMe x = x + x

--double two numbers and add them together
doubleUs x y = (doubleMe x) + (doubleMe y)

-- find a small number 
doubleSmallNumber x = if x > 100
			then x
			else doubleMe x

--replace values with BOOM and BANG
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

-- find length of list
length' xs = sum[1 | _ <- xs]

-- remove non uppercase from string and return only uppercase
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]


