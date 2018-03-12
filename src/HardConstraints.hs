module HardConstraints where

--Function can be used to determine if there is a forced partial assignment in the forbidden list
--returns True if there isn't a forced partial assignment in the forbidden list and False otherwise
--Takes in forced partial assignment and forbidden list as parameters
norepeatCheck :: (Eq a) => [a] -> [a] -> Bool
norepeatCheck [] _ = True
norepeatCheck (x:xs) ts
    |elem x ts = False
    |otherwise = norepeatCheck xs ts


--Checks if there are any repeated assignments in the forced partial assignments, which is passed in as a parameter
--returns True if there isn't a repeat, False otherwise
norepeatCheckself :: (Eq a) => [a] -> Bool
norepeatCheckself [] = True
norepeatCheckself (x:xs)
    |elem x xs = False
    |otherwise = norepeatCheckself xs

