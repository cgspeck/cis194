module Golf where
  skips :: [a] -> [[a]]
  skips x = map (takeEvery x 1) [1..length x]

  takeEvery :: [a] -> Int -> Int -> [a]
  takeEvery (x:xs) curPos takePos
    | (mod curPos takePos) == 0 = x : takeEvery xs (curPos + 1) takePos
    | otherwise = takeEvery xs (curPos + 1) takePos
  takeEvery [] _ _ = []

  localMaxima :: [Integer] -> [Integer]
  localMaxima (h:ms@(m:xs@(n:t)))
    | m > h && m > n = [m] ++ localMaxima xs
    | otherwise = localMaxima ms
  localMaxima _ = []

  histogram :: [Integer] -> String
  -- histogram xs = _countToLine ( _findCount xs 0 ) 0
  histogram xs = unlines $ map(_countToLine(_findCounts xs)) (reverse [1..maximum (_findCounts xs)]) ++ _boilerplate

  -- stuff that goes at the bottom, i.e. --- and column labels
  _boilerplate :: [String]
  _boilerplate = ["==========", "0123456789"]

  _findCounts :: [Integer] -> [Integer]
  _findCounts xs = map (_findCount xs) [0..9]

  -- look through [x] and count digits equal to num_to_tally
  _findCount :: [Integer] -> Integer -> Integer
  _findCount xs num_to_tally = toInteger ( length (filter (\x -> x == num_to_tally) xs))

  --   -- look through [x], if x >= threshold return a '*' otherwise a ' '
  _countToLine :: [Integer] -> Integer -> [Char]
  _countToLine (x:xs) threshold
    | x >= threshold = '*' : _countToLine xs threshold
    | otherwise = ' ' : _countToLine xs threshold
  _countToLine [] _ = []
