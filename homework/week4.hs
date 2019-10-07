import Data.List

fun_1 :: [Integer] -> Integer
fun_1 [] = 1
fun_1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1 :: [Integer] -> Integer
-- fun1 xs = foldl (subtract 2) 1 (filter(even xs))
-- fun1 xs = foldl'  ((*) . (\x -> x - 2)) 1 (filter even xs)
fun1 = foldl' ((*) . subtract 2) 1 . filter even

-- hint: iterate and takeWhile
fun_2 :: Integer -> Integer
fun_2 1 = 0
fun_2 n
  | even n = n + fun_2 (n `div` 2)
  | otherwise = fun_2 (3 * n + 1)
