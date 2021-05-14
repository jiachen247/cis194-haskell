-- Exercise 1
toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0 = []
    | otherwise = toDigits (n `div` 10) ++  (n `mod` 10) : []

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

-- toDigits 1234 == [1,2,3,4]
-- toDigitsRev 1234 == [4,3,2,1]
-- toDigits 0 == []
-- toDigits (-17) == []

-- Exercise 2
doubleEveryOtherFromFront :: [Integer] -> [Integer]
doubleEveryOtherFromFront [] = []
doubleEveryOtherFromFront (x:y:t) = x : y * 2 : (doubleEveryOtherFromFront t)
doubleEveryOtherFromFront [x] = x : []

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther lst = reverse $ doubleEveryOtherFromFront (reverse lst) 

-- doubleEveryOther [8,7,6,5] == [16,7,12,5]
-- doubleEveryOther [1,2,3] == [1,4,3]

-- Exercise 3
sumDigit :: Integer -> Integer
sumDigit n = sum (toDigits n)

sumDigits :: [Integer] -> Integer
sumDigits lst = sum (map sumDigit lst)

-- sumDigits [16,7,12,5] == 22

-- Exercise 4
validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther $ toDigits n) `mod` 10 == 0

-- validate 4012888888881881 == True
-- validate 4012888888881882 == False

-- Exercise 5
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n src dest alt = (hanoi (n-1) src alt dest) ++ [(src, dest)] ++ (hanoi (n-1) alt dest src)

-- hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]

-- Exercise 6
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n src dest alt1 alt2
    | n == 0 = []
    | n == 1 = [(src, dest)]
    | otherwise = (hanoi4 (n-2) src alt1 alt2 dest) ++ [(src, alt2)] ++ [(alt2, dest)] ++ (hanoi4 (n-2) alt1 dest src alt2)

-- length $ hanoi 15 "a" "b" "c" == 32767
-- length $ hanoi4 15 "a" "b" "c" "d" == 382