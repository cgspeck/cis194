type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n source dest temp =
    let nSubtractOne = n -1
    in hanoi nSubtractOne source temp dest ++
       [(source, dest)] ++
       hanoi nSubtractOne temp dest source
