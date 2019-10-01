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
