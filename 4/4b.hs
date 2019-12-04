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

hasDoubleButNotTriple [a,b,c,d,e,f] = or [t1,t2,t3,t4,t5]
    where
    t1 = a == b && b /= c
    t2 = b == c && a /= b && c /= d
    t3 = c == d && b /= c && d /= e
    t4 = d == e && c /= d && e /= f
    t5 = e == f && d /= e          

numValidPasswords xmin xmax = length $ filter (\x -> (isMonotonic x) && (hasDoubleButNotTriple x)) $ map digitsToList [xmin .. xmax]
