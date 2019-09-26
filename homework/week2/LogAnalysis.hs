{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage v = case words v of
    ("I":ts:rest) -> LogMessage Info (read ts) (unwords rest)
    ("W":ts:rest) -> LogMessage Warning (read ts) (unwords rest)
    ("E":severity:ts:rest) -> LogMessage (Error (read severity)) (read ts) (unwords rest)
    _ -> Unknown v

parse :: String -> [LogMessage]
{-parse v = (map parseMessage) (lines v)
parse v = (map parseMessage . lines) v
parse = (map parseMessage . lines)-}
parse = map parseMessage . lines

{-
a = parseMessage  "I 1 A"
b = parseMessage  "I 2 B"
c = parseMessage  "I 3 C"
d = parseMessage  "I 4 D"
e = parseMessage  "I 5 E"
f = parseMessage  "I 6 F"
build [a, b, c, d, e, f]
-}
insert :: LogMessage -> MessageTree -> MessageTree
{- insert a b = case a of
    Unknown _ -> b
    LogMessage _ _ _ -> Node b a _2
    -}
{-
build [a, b, c, d, e, f]
mt
-}
insert (Unknown _) a = a
-- insert a@(LogMessage _ _ _) Leaf = Node Leaf a Leaf
-- insert a@(LogMessage _ _ _) b@(Node _ _ _) = Node b a Leaf
-- insert (Unknown _) b = b
-- insert a@(LogMessage _ _ _) Leaf = Node Leaf a Leaf
-- insert a@(LogMessage _ ts _) (Node x y1 z) = _
-- insert a@(LogMessage _ _ _) b@(Node _ _ _) = Node b a Leaf
insert x@(LogMessage _ _ _)     Leaf = _singleton x

-- insert i@(LogMessage _   b  _)  (Node d    j@(LogMessage _ f _) h) = _
insert x@(LogMessage _ x_ts _)  (Node left a@(LogMessage _ a_ts _) right)
    | x_ts < a_ts = Node (insert x left) a right
    | otherwise = Node left a (insert x right)

insert _       (Node _ (Unknown _) _) = undefined


_singleton :: LogMessage -> MessageTree
_singleton x = Node Leaf x Leaf

build :: [LogMessage] -> MessageTree
-- build x = _build Leaf x
-- build x = foldr insert Leaf x
build = foldr insert Leaf

_build :: MessageTree -> [LogMessage] -> MessageTree
_build Leaf (h:t) = Node Leaf h (_build Leaf t)
_build Leaf [] = Leaf
_build a@(Node _ _ _) (h:t) = Node a h (_build Leaf t)
_build a@(Node _ _ _) [] = a

{-
a = parseMessage  "I 10 Foo"
e = parseMessage  "I 20 Bor"
c = parseMessage  "I 30 Boz"
b = parseMessage  "I 40 Qox"
e = parseMessage  "I 50 Qax"
mt = build [a, b, c, d, e]
inOrder mt
-}
-- transform pre-sorted Message to list of log messages
inOrder :: MessageTree -> [LogMessage]
-- inOrder (Node Leaf b Leaf) = [b]
-- inOrder (Node Leaf b c@(Node _ _ _)) = b : (inOrder c)
-- inOrder (Node (Node _ _ _) b c@(Node _ _ _)) = b : (inOrder c)
-- inOrder (Node (Node _ _ _) b Leaf) = [b]
inOrder (Leaf) = []
inOrder (Node left cur right) = (inOrder left) ++ [cur] ++ (inOrder right)

whatWentWrong :: [LogMessage] -> [String]
-- whatWentWrong = (inOrder . build . relevanceFilter)
-- whatWentWrong x = map _ (inOrder . build ( relevanceFilter(x)))
-- whatWentWrong = (relevanceFilter build) inOrder
-- map _ (inOrder . build ( relevanceFilter(x)))
whatWentWrong xs = getLogMessage <$> filter isRelevant (inOrder . build $ xs)

relevanceFilter :: [LogMessage] -> [LogMessage]
relevanceFilter ( h@(LogMessage (Error sev) _ _) : t )
  | sev >= 50 = [h] ++ relevanceFilter t
  | otherwise = relevanceFilter t
relevanceFilter ( (LogMessage _ _ _) :t ) = relevanceFilter t
relevanceFilter ( (Unknown _ ) : t ) = relevanceFilter t
relevanceFilter [] = []

isRelevant :: LogMessage -> Bool
isRelevant (LogMessage (Error sev) _ _) = sev >= 50
isRelevant _ = False

getLogMessage :: LogMessage -> String
getLogMessage (LogMessage _ _ msg) = msg
getLogMessage (Unknown msg) = msg
