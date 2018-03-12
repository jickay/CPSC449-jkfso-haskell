module SoftConstraints where

--Irrelevant?
takeEight :: Int -> [Int] -> [Int]
takeEight x [] = if x /= 0 then error "Empty List" else []
takeEight 0 a = a
takeEight x a = 
    let 
        one = take x a
    in one

--Irrelevant?
mklst :: [Int] -> [[Int]]
mklst x = 
    let mach1 = takeEight 8 x
        minusMach1 = drop 8 x
        mach2 = takeEight 8 minusMach1
        minusMach2 = drop 8 minusMach1
        mach3 = takeEight 8 minusMach2
        minusMach3 = drop 8 minusMach2
        mach4 = takeEight 8 minusMach3
        minusMach4 = drop 8 minusMach3
        mach5 = takeEight 8 minusMach4
        minusMach5 = drop 8 minusMach4
        mach6 = takeEight 8 minusMach5
        minusMach6 = drop 8 minusMach5
        mach7 = takeEight 8 minusMach6
        minusMach7 = drop 8 minusMach6
        mach8 = takeEight 8 minusMach7
        minusMach8 = drop 8 minusMach7
    in mach1:mach2:mach3:mach4:mach5:mach6:mach7:mach8:[]

--Gets an item in a list based on a given idex
get :: Int -> [a] -> a
get x a = 
    let 
        revA = reverse a
        len = length a
        numDrop = len - x 
        first = take numDrop revA
        rev = reverse first
        item = head rev
    in item

--Gets the first item in a tuple
getFirstInTuple :: (a, b, c) -> a
getFirstInTuple (x, _, _) = x

--Gets the second item in a tuple
getSecondInTuple :: (a,b,c) -> b
getSecondInTuple (_, y, _) = y

--Gets the third item in a tuple
getThirdInTuple :: (a,b,c) -> c
getThirdInTuple (_, _, z) = z

--Checks to see if an item is in a list
checkInList :: (Ord a) => a -> [a] -> Bool
checkInList item list
    | null list == True = False
    | headList == item  = True
    | otherwise         = checkInList item restList
    where   restList    = drop 1 list
            headList    = head list

--Irrelevant?
--takes two args, the second should always be an empty list
tupleListToTuple :: Int -> [(Char, Char, Int)] -> [Char] -> [Char]
tupleListToTuple whichChar tupleList returnList
    | null tupleList == True    = returnList
    | whichChar == 0            = tupleListToTuple whichChar restTupleList newFirstReturnList
    | whichChar == 1            = tupleListToTuple whichChar restTupleList newSecondReturnList
    | otherwise                 = []
    where   restTupleList = drop 1 tupleList
            firstTuple = head tupleList
            firstInTuple = getFirstInTuple firstTuple
            secondInTuple = getSecondInTuple firstTuple
            newFirstReturnList = firstInTuple:returnList
            newSecondReturnList = secondInTuple:returnList

--Compares a char to the first element in a (char, char, Int) tuple
compareToFirstInTuple :: Char -> (Char, Char, Int) -> Bool
compareToFirstInTuple item tuple
    | item == tuple0    = True
    | otherwise         = False
    where 
        tuple0 = getFirstInTuple tuple

--Compares a char to the second element in a (char, char, Int) tuple
compareToSecondInTuple :: Char -> (Char, Char, Int) -> Bool
compareToSecondInTuple item tuple
    | item == tuple1    = True
    | otherwise         = False
    where 
        tuple1 = getSecondInTuple tuple

--Char = Letter to check
--[Char] = List of current matches
--[(Char, Char, Int)] = List of tooNearTasks
--Int = Running total of penalty
--Int = Return penalty
--Calculates the additional penalty when given a task, current matches, and the tooNearPenalty list
--Fourth argument is to keep track of penalty through the recursion
tooNearPen :: Char -> [Char] -> [(Char, Char, Int)] -> Int -> Int
tooNearPen task currentMatches tooNearList penalty
    | null tooNearList == True      = penalty
    | compareToFirstInTuple task firstInTooNearList == True = tooNearPen task currentMatches restTooNearList secondPenalty
    | compareToSecondInTuple task firstInTooNearList == True = tooNearPen task currentMatches restTooNearList firstPenalty
    | otherwise                     = tooNearPen task currentMatches restTooNearList penalty
    where 
        firstInTooNearList = get 0 tooNearList
        restTooNearList = drop 1 tooNearList
        rightTupleTask = getSecondInTuple firstInTooNearList
        leftTupleTask = getFirstInTuple firstInTooNearList
        newSecondPenalty = getSecondPenalty currentMatches firstInTooNearList
        newFirstPenalty = getFirstPenalty currentMatches firstInTooNearList
        secondPenalty = newSecondPenalty + penalty
        firstPenalty = newFirstPenalty + penalty

--Gets the additional penalty if the second char in the tuple is in the current matches list
getSecondPenalty :: [Char] -> (Char, Char, Int) -> Int
getSecondPenalty matches taskTuple
    | checkInList task matches == True  = getThirdInTuple taskTuple
    | otherwise                         = 0
        where 
            task = getSecondInTuple taskTuple

--Gets the additional penalty if the first char in the tuple is in the current matches list
getFirstPenalty :: [Char] -> (Char, Char, Int) -> Int
getFirstPenalty matches taskTuple
    | checkInList task matches == True  = getThirdInTuple taskTuple
    | otherwise                         = 0
        where 
            task = getFirstInTuple taskTuple
