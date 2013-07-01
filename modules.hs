import Data.List
import Data.Function
import Data.Char
import qualified Data.Map as Map

main = do putStrLn "Data.map program!\n"
          putStrLn "Finding betty:"
          putStrLn $ show $ findKey "betty" phoneBook

phoneBook =   
    [("betty","555-2938")  
    ,("bonnie","452-2928")  
    ,("patsy","493-2928")  
    ,("lucille","205-2928")  
    ,("wendy","939-8282")  
    ,("penny","853-2492")  
    ]  

--finds the number of uniques values in the list passed
numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

--faren to celc conversion function
f_to_c :: (Fractional a) => a -> a
f_to_c x = (x-32) * (5.0 / 9.0)

--searching a list for a sublist
search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack = 
  let nlen = length needle
  in foldl 
  (\acc x -> if take nlen x == needle then True else acc)
  False 
  (tails haystack)
--shift the letters in the string given by a certain amount
encode :: Int -> String -> String
encode shift msg =
  let ords = map ord msg
      shifted = map (+ shift) ords
  in map chr shifted
--decode function for encoded function above
decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg

--lookup of association list by key
findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key [] = Nothing
findKey key ( (k,v):xs ) = if key == k
            then Just v
            else findKey key xs

--lookup of association list by folds
findKey' :: (Eq k) => k -> [ (k,v) ] -> Maybe v
findKey' key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing
