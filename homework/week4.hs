
{--
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs
--}

-- don't forget to do the multiplication once this is working
fun1 :: [Integer] -> Integer
fun1 = foldl 1 (subtract 2) takeWhile(even)