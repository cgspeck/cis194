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
  histogram = undefined

  -- stuff that goes at the bottom, i.e. --- and column labels
  _boilerplate :: [Char]
  _boilerplate = undefined

  -- look through [x] and count digits equal to num_to_tally
  _findCount :: [Integer] -> Int -> Int
  _findCount [x:xs] num_to_tally = _

    -- look through [x], if x > threshold return a '*' otherwise a ' '
  _countToLine :: [Integer] -> Int -> [Char]
  _countToLine [x:xs] threshold = _
