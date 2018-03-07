module Parser2 where
import Data.Char
import Data.List

------------------------------- FULL PARSERS -------------------------------
--getMTPairs :: [String] -> [String]
--getTTPairs :: 


--when parsing multiple tuples, **MAP** the tuple segment parsers over the [String]

------------------------------- TUPLE SEGMENT PARSERS -------------------------------

--working
stringToMTPair :: String -> (Int, Char)
stringToMTPair a = if isValidMTPair (removeEndSpaces a)
    then (digitToInt (a !! 1), a !! 3)
    else (-1,'Z')

--pass a full line, returns a 
--working
stringToTTPair :: String -> (Char, Char)
stringToTTPair a = if isValidTTPair (removeEndSpaces a)
    then (a !! 1, a !! 3)
    else ('Z','Z')

--stringToPenaltyRow :: 

------------------------------- ERROR HANDLING -------------------------------
--checkForError :: String -> String
--checkForError (xs) = case (take 2 xs) of 
--    "ER" = drop 4 xs
--    _ = "none"




------------------------------- INPUT SPLITTING/GETTERS -------------------------------

--removeSpaceLines :: [String] -> [String]
--removeSpaceLines [] = []
--removeSpaceLines (x:xs) if (allSpaces x) then removeSpaceLines xs else x:(removeSpaceLines xs)

--getLinesBetween :: [Char] -> [Char] -> [String]
--getLinesBetween a b = linesAfter (linesBefore b) a --"between but not including a and b"

--linesAfter :: [String] -> [Char] -> [String]
--linesAfter xs y = if (y `elem` xs) then drop ((y `elemIndex` xs)+1) xs else ["ER: Error while parsing input file"]

--linesBefore :: [String] -> [Char] -> [String]
--linesBefore xs y = if (y `elemIndex` xs) then take ((y `elemIndex` xs)+1) xs else ["ER: Error while parsing input file"]








------------------------------- PROTOTYPE FUNCTIONS -------------------------------

isValidPenaltyRow :: -> String -> Bool
isValidPenaltyRow --TODO: Pattern match to 





------------------------------- WHITESPACE HANDLING -------------------------------

--working
removeEndSpaces :: [Char] -> [Char]
removeEndSpaces (x:xs) = if allSpaces xs then [x] else x:(removeEndSpaces xs)

--working
allSpaces :: [Char] -> Bool
allSpaces [] = True
allSpaces (x:xs) = if (x == ' ') then allSpaces xs else False

------------------------------- BOOL VALIDITY CHECKS -------------------------------

isValidTTPTriple :: [Char] -> Bool
isValidTTPTriple ['(',x,',',y,',',z,')'] = 
    (isValidTask x) && (isValidTask y) && (isValidPenaltyALT z) --TODO: See isValidPenaltyALT func
isValidTTPTriple b = False

--TODO: This check may be redundant; can map if structuring penalties 
isValidMTPTriple :: [Char] -> Bool
isValidMTPTriple ['(',x,',',y,',',z,')'] = 
    (isValidMachine x) && (isValidTask y) && (isValidPenaltyALT z) --TODO: See isValidPenaltyALT func
isValidMTPTriple b = False

isValidTTPair :: [Char] -> Bool
isValidTTPair ['(',x,',',y,')'] = ((isValidTask x) && (isValidTask y))
isValidTTPair b = False

isValidMTPair :: [Char] -> Bool
isValidMTPair ['(',x,',',y,')'] = ((isValidMachine x) && (isValidTask y))
isValidMTPair b = False

isValidPenaltyALT :: Char -> Bool --TODO: only works on single digits
isValidPenaltyALT a
    | (b >= 0) = True
    | otherwise = False 
    where b = digitToInt a

isValidPenalty :: [Char] -> Bool
isValidPenalty [] = False
isValidPenalty [x] = isDigit x
isValidPenalty (x:xs) = if isDigit x then isValidPenalty xs else False

isValidMachine :: Char -> Bool
isValidMachine a 
    | isDigit a = (digitToInt a) `elem` [1..8]
    | otherwise = False

isValidTask :: Char -> Bool
isValidTask a 
    | a `elem` ['A'..'H'] = True
    | otherwise = False