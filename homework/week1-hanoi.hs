type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n pa pb pc 
  | n == 1 = []
  | otherwise = (pa, pb) : (pa, pc) : (pb, pc) : hanoi (n -1) pa pb pc

-- _hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
-- _hanoi n pa pb pc 
--   | n == 0 = []
--   | otherwise = (pa, pb) : hanoi (n -1) pa pb pc
