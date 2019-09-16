type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 source dest _ = [(source, dest)]
hanoi n source dest temp =
    let nSubtractOne = subtract 1 n
    in hanoi nSubtractOne source temp dest ++
       hanoi 1 source dest temp ++
       hanoi nSubtractOne temp dest source
