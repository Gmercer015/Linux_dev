data Point = Point Float Float deriving (Show)

data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

--surface area of object
surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

--move a shape at x,y by a,b
nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b) ) 

--creates circle with default location at 0,0
baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

--simple functions that use a base shape value
baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

--first name, last name, age, height, phone number, ice-cream flavor
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Eq, Show)
--also able to use algebraic data types to make enumerations
data Day = Monday | Tuesday | Wednesday | Thursday  | Friday | Saturday | Sunday deriving (Eq, Ord, Show, Read, Bounded, Enum)

--type is the same as typedef or renaming a value to convey information
type PhoneBook = [(String,String)]

phoneBook :: PhoneBook
phoneBook =
    [("better", "555-2983")
    ,("bonnieBitch","452-999")
    ]

--paramterized type synonyms
type AssocList k v = [(k,v)]

--cool data type that can represent either value
data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)

--implementing our own list!
infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

infixr 5 .++ 
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

--working on type class creation, data we will write our own typeclasses for
data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False


class Eq a where
    (==)
