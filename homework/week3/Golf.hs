module Golf where
  skips :: [a] -> [[a]]
  skips = undefined
  
  takeEvery :: [a] -> Integer -> Integer -> [a]
  takeEvery (x:xs) takePos curPos
    | (mod curPos takePos) == 0 = x : takeEvery xs takePos (curPos + 1)
    | otherwise = takeEvery xs takePos (curPos + 1)
  takeEvery [] _ _ = []
