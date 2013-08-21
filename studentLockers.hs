iport qualified Data.Map as Map

data LockerState = Taken | Free deriving (Show, Eq)

--type used to change String to Code for understanding 
type Code = String

--map help for student lookups
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
    case Map.lookup lockerNumber map of
        Nothing -> Left $ "Locker number" ++ show lockerNumber ++ " doesn't exist!"
        Just (state, code) -> if state /= Taken
                                then Right code
                                else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

lockers :: LockerMap
lockers = Map.fromList
    [(100,(Taken,"ZD39I"))
    ,(101,(Free,"JKE45"))
    ,(103,(Free,"GHI33"))
    ,(105,(Free,"QOT34"))
    ,(109,(Taken,"89THG"))
    ,(110,(Taken,"99292"))
    ]
