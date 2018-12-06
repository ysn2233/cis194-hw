-- CIS194 HW1
-- author: Yu Shengnan

-- Exercise 1
toDigits :: Integer -> [Integer]
toDigits x = if x <= 0 then [] else toDigits (x `div` 10) ++ [(x `mod` 10)]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = if x <= 0 then [] else (x `mod` 10) : toDigitsRev (x `div` 10)


-- Exercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:xs) = if odd (length xs) then x : y*2 : doubleEveryOther xs else x*2 : y : doubleEveryOther xs


-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x] = sum (toDigits x)
sumDigits (x:xs) =  sumDigits [x] + sumDigits xs


-- Exercise 4
validate :: Integer -> Bool
validate x = sumDigits (doubleEveryOther (toDigits x)) `mod` 10 == 0


-- Exercise 5
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 a b c = []
hanoi n a b c = hanoi (n - 1) a c b ++ ((a,b) : hanoi (n - 1) c b a)


-- Exercise 6
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 a b c d = []
hanoi4 1 a b c d = hanoi 1 a b c
hanoi4 n a b c d = hanoi4 (n - 2) a c b d ++ [(a, d), (a, b), (d, b)] ++ hanoi4 (n - 2) c b a d
