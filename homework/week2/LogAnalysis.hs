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
