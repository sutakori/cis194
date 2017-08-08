toDigitsRev :: Integer -> [Integer]
toDigitsRev n | n > 0 = (mod n 10) : toDigitsRev (div n 10)
              | otherwise = []

toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse (doubleEveryOtherRev xsRev)
  where xsRev = reverse xs
        doubleEveryOtherRev :: [Integer] -> [Integer]
        doubleEveryOtherRev [] = []
        doubleEveryOtherRev (x : []) = (x : [])
        doubleEveryOtherRev (x : y : xs) = (x : 2*y : doubleEveryOtherRev xs)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (s : xs) = (sum (toDigitsRev s)) + (sumDigits xs)

validate :: Integer -> Bool
validate n | characteristic == 0 = True
           | characteristic /= 0 = False
  where characteristic = mod (sumDigits (doubleEveryOther (toDigits n))) 10
