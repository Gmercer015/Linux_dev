import qualified Data.Map as Map                        --import Map functions and data

data LockerState = Taken | Free deriving (Show, Eq)     --LockerState to test if taken or free

type Code = String                                      --change name of string to Code for reading purposes

type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code  --check if locker is free taken
lockerLookup lockerNumber map =
    case Map.lookup lockerNumber map of                 --case if locker is in the map
        Nothing -> Left $ "Locker number" ++ show lockerNumber ++ " doesn't exist!"
        Just (state, code) -> if state /= Taken         --if locker exists, check if taken or not
                                then Right code
                                else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"
lockers :: LockerMap                                    --locker numbers of students
lockers = Map.fromList
    [(100,(Taken,"ZD39I"))
    ,(101,(Free,"JKE45"))
    ,(103,(Free,"GHI33"))
    ,(105,(Free,"QOT34"))
    ,(109,(Taken,"89THG"))
    ,(110,(Taken,"99292"))
    ]

main :: IO ()
main = do putStrLn "Check your locker number: "         --prompt user for locker number
          x <- readLn                                   --read in integer from command prompt
          putStrLn $ show $ lockerLookup x lockers      --put result of function to screen
          
