module Golf where
  skips :: [a] -> [[a]]
  skips x = map takeEvery x 1 [1..length x]
  skips _ = []
  
  takeEvery :: [a] -> Integer -> Integer -> [a]
  takeEvery (x:xs) curPos takePos 
    | (mod curPos takePos) == 0 = x : takeEvery xs takePos (curPos + 1)
    | otherwise = takeEvery xs takePos (curPos + 1)
  takeEvery [] _ _ = []
