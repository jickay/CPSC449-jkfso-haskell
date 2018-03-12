module Parser where
import Data.Char
import Data.List

------------------------------- CONFLICT CHECKER -------------------------------

--isForcedAssignmentConflict :: [(Int,Char)] -> String
--isForcedAssignmentConflict 


------------------------------- LINE FETCHERS -------------------------------

fetchName :: [String] -> [String]
fetchName x = linesBetween x "Name:" "forced partial assignment:"

fetchForcedPartialAssignments :: [String] -> [String]
fetchForcedPartialAssignments x = linesBetween x "forced partial assignment:" "forbidden machine:"

fetchForbiddenMachines :: [String] -> [String]
fetchForbiddenMachines x = linesBetween x "forbidden machine:" "too-near tasks:"

fetchTooNearTasks :: [String] -> [String]
fetchTooNearTasks x = linesBetween x "too-near tasks:" "machine penalties:"

fetchMachinePenalties :: [String] -> [String]
fetchMachinePenalties x = linesBetween x "machine penalties:" "too-near penalities"

fetchTooNearPenalties :: [String] -> [String]
fetchMachinePenalties x = linesAfter x "too-near penalities"


------------------------------- GENERAL DOCUMENT SEPARATORS -------------------------------

linesAfter :: [String] -> String -> [String]
linesAfter (x:xs) b = if ((removeEndSpaces x) == b) then xs else linesAfter xs b
linesAfter x b = if (x!!1 == b) then [] else ["ER Error while parsing input file"] --TODO: maybe change error

linesBefore :: [String] -> String -> [String]
linesBefore (x:xs) b = if ((removeEndSpaces x) == b) then [] else x:(linesBefore xs b)
linesBefore x b = if (x!!1 == b) then [] else ["ER Error while parsing input file"] --TODO: maybe change error

linesBetween :: [String] -> String -> String -> [String] 
linesBetween x a b = linesBefore (linesAfter x a) b  


------------------------------- MULTILINE PARSERS -------------------------------

parseMTPairs :: [String] -> [(Int, Char)]
parseMTPairs a = map stringToMTPair a

parseTTPairs :: [String] -> [(Char, Char)]
parseTTPairs a = map stringToTTPair a

--parseTTPTriples
--parse

------------------------------- SINGLE LINE PARSERS -------------------------------

stringToMTPair :: String -> (Int, Char)
stringToMTPair a = if (isValidMTPair (removeEndSpaces a) == "PASS")
    then (digitToInt (a !! 1), a !! 3)
    else (-1,'Z')

stringToTTPair :: String -> (Char, Char)
stringToTTPair a = if (isValidTTPair (removeEndSpaces a) == "PASS")
    then (a !! 1, a !! 3)
    else ('Z','Z')

stringToTTPTriple :: String -> (Char, Char, Int)
stringToTTPTriple a = if (isValidTTPTriple (removeEndSpaces a) == "PASS")
    then (a !! 1, a!!3, digitToInt(a !! 5)) --TODO: that '5' only works on single digits
    else ('Z','Z',-1)

--
--penaltyLineToTuple :: String -> (Int, Int, Int, Int, Int, Int, Int, Int)
--penaltyLineToTuple ('(':a) = if ((isValidPenaltyRow a) == "PASS") then 


------------------------------- SEGMENT VALIDATORS -------------------------------

--TODO: Currently only works on single-digits in both pattern-mathing and in function usage
isValidPenaltyRow :: [Char] -> String
isValidPenaltyRow (a:' ':b:' ':c:' ':d:' ':e:' ':f:' ':g:' ':h:xs) = 
    if (not (isValidPenalty a)) 
        || (not (isValidPenalty b)) 
        || (not (isValidPenalty c)) 
        || (not (isValidPenalty d)) 
        || (not (isValidPenalty e))
        || (not (isValidPenalty f))
        || (not (isValidPenalty g))
        || (not (isValidPenalty h))
    then "ER invalid penalty" else "PASS"
isValidPenaltyRow z = "ER machine penalty error"

isValidTTPTriple :: [Char] -> String
isValidTTPTriple ['(',x,',',y,',',z,')'] = 
    if ((isValidTask x) && (isValidTask y) && (isValidPenalty z)) then "PASS" else "ER invalid task" 
        --TODO: See isValidPenalty func
isValidTTPTriple z = "ER Error while parsing input file"

isValidTTPair :: [Char] -> String
isValidTTPair ['(',x,',',y,')'] = 
    if ((isValidTask x) && (isValidTask y)) then "PASS" else "ER invalid machine/task"
isValidTTPair z = "ER Error while parsing input file"

isValidMTPair :: [Char] -> String
isValidMTPair ['(',x,',',y,')'] = 
    if ((isValidMachine x) && (isValidTask y)) then "PASS" else "ER invalid machine/task"
isValidMTPair z = "ER Error while parsing input file"

------------------------------- WHITESPACE HANDLING -------------------------------

removeEndSpaces :: [Char] -> [Char]
removeEndSpaces (x:xs) = if allSpaces xs then [x] else x:(removeEndSpaces xs)

allSpaces :: [Char] -> Bool
allSpaces [] = True
allSpaces (x:xs) = if (x == ' ') then allSpaces xs else False


------------------------------- BOOL VALUE VALIDITY CHECKS -------------------------------

--isValidPenalty :: [Char] -> Bool
--isValidPenalty [] = False
--isValidPenalty [x] = isDigit x
--isValidPenalty (x:xs) = if isDigit x then isValidPenalty xs else False

isValidPenalty :: Char -> Bool --TODO: only works on single digits
isValidPenalty a
    | (b >= 0) = True
    | otherwise = False 
    where b = digitToInt a

isValidMachine :: Char -> Bool
isValidMachine a 
    | isDigit a = (digitToInt a) `elem` [1..8]
    | otherwise = False

isValidTask :: Char -> Bool
isValidTask a 
    | a `elem` ['A'..'H'] = True
    | otherwise = False