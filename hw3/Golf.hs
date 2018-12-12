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

occ :: Eq a => [a] -> a -> Integer
occ (x:xs) y = if x == y then (1 + occ xs x) else occ xs y 
occ _ _ = 0

star :: Integer -> String -> String
star x s = (if x > 0 then '*' else ' '):s

line :: [Integer] -> String
line o
    | all (<=0) o = "==========\n0123456789"
    | otherwise = foldr star ('\n': (line (map (subtract 1) o))) o

histogram :: [Integer] -> String
histogram x = line [occ x n | n <- [0..9]]
