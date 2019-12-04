main = print $ show $ numValidPasswords 109165 576723

digitsToList x 
    | x < 10 = [x]
    | otherwise = (digitsToList m) ++ [d]
    where (m, d) = x `divMod` 10

isMonotonic [] = True
isMonotonic [x] = True
isMonotonic (x:y:xs) = x <= y && isMonotonic (y:xs)

hasRepeat [] = False
hasRepeat [x] = False
hasRepeat (x:y:xs) = x == y || hasRepeat (y:xs)

numValidPasswords xmin xmax = length $ filter (\x -> (isMonotonic x) && (hasRepeat x)) $ map digitsToList [xmin .. xmax]
