toDigits       :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

toDigitsRev    :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = n `mod` 10 : toDigitsRev(n `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . _doubleEveryOther . reverse

_doubleEveryOther :: [Integer] -> [Integer]
_doubleEveryOther (h:m:t) = h : (2 * m) : _doubleEveryOther(t)
_doubleEveryOther t = t

sumDigits :: [Integer] -> Integer
-- sumDigits v = sum(concat (map toDigits v))
-- sumDigits v = (sum . concatMap toDigits) v
sumDigits = sum . concatMap toDigits

validate :: Integer -> Bool
validate n = _validate n == 0

_validate :: Integer -> Integer
_validate n = mod (sumDigits ( doubleEveryOther (toDigits n))) 10
-- _validate n = mod 10  (sumDigits ( doubleEveryOther (toDigits n))) 10
