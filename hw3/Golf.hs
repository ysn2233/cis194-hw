-- Exercise 1 Hopscotch

everyN :: [a] -> Int -> Int -> [a]
everyN [] _ _ = []
everyN (x:xs) c n = if c == n then x: everyN xs 1 n else everyN xs (c + 1) n
    
skips :: [a] -> [[a]]
-- skips x = [ everyN x n 1 | n <- [1..length x]]
skips x = map (everyN x 0) [0..length x]


-- Exercise 2 Local maxima

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:xs)
    | y > x && y > z = y: localMaxima (z:xs)
    | otherwise = localMaxima (y:z:xs)
localMaxima arr = []


-- Exercise 3 Histogram
data Count = Count Int Int   

occurance :: Eq a => [a] -> a -> Integer
occurance (x:xs) y = if x == y then (1 + occurance xs x) else occurance xs y 
occurance _ _ = 0

occurList x = [occurance x n | n <- [0..9]]

drawStar :: Integer -> String -> String
drawStar x str = (if x > 0 then '*' else ' '):str

histogram :: [Integer] -> String
histogram occ
    | all (<=0) occ = "==========\n0123456789"
    | otherwise = foldr drawStar ('\n': (drawLine (map (subtract 1) occ))) occ
