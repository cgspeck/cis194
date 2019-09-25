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

insert :: LogMessage -> MessageTree -> MessageTree
{- insert a b = case a of
    Unknown _ -> b
    LogMessage _ _ _ -> Node b a _2
    -}

insert (Unknown _) a = a
-- insert a@(LogMessage _ _ _) Leaf = Node Leaf a Leaf
-- insert a@(LogMessage _ _ _) b@(Node _ _ _) = Node b a Leaf
-- insert (Unknown _) b = b
-- insert a@(LogMessage _ _ _) Leaf = Node Leaf a Leaf
-- insert a@(LogMessage _ ts _) (Node x y1 z) = _
-- insert a@(LogMessage _ _ _) b@(Node _ _ _) = Node b a Leaf
insert x@(LogMessage _ _ _) Leaf = _singleton x
insert x@(LogMessage _ x_ts _) (Node left a@(LogMessage _ a_ts _) right)
    | x_ts == a_ts = Node left x right
    | x_ts < a_ts = Node (insert x left) a right
    | x_ts > a_ts = Node left a (insert x right)
insert (LogMessage _ _ _) (Node _ (Unknown _) _) = undefined
insert (LogMessage _ _ _) (Node _ (LogMessage _ _ _) _) = undefined

_singleton :: LogMessage -> MessageTree
_singleton x = Node Leaf x Leaf

build :: [LogMessage] -> MessageTree
-- build x = _build Leaf x
build x = foldr insert Leaf x

_build :: MessageTree -> [LogMessage] -> MessageTree
_build Leaf (h:t) = Node Leaf h (_build Leaf t)
_build Leaf [] = Leaf
_build a@(Node _ _ _) (h:t) = Node a h (_build Leaf t)
_build a@(Node _ _ _) [] = a

inOrder :: MessageTree -> [LogMessage]
inOrder (Leaf) = undefined
inOrder (Node _ b c) = b : inOrder c
