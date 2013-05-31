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
second (_, y, _)

--third element of triple tuple
third :: (a, b, c) -> c
third (_, _, z) = z
