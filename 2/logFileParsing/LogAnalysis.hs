module LogAnalysis where

import Data.Char
import Log

isNum :: String -> Bool
isNum = all isDigit

toNum :: String -> Int
toNum s = sum [x * 10 ^ y | (x , y) <- zip (reverse (map digitToInt s))  [0..length s - 1]]

{-parse an individual message-}
parseMessage :: String -> LogMessage
parseMessage str = parseWord (words str)
  where parseWord word = case word of
          ("E" : n : t : ws) | (isNum n) && (isNum t) -> LogMessage (Error (toNum n)) (toNum t) (drop (length n + length t + 4) str)
          ("I" : t : ws) | isNum t -> LogMessage Info (toNum t) (drop (length t + 3) str)
          ("W" : t : ws) | isNum t -> LogMessage Warning (toNum t) (drop (length t + 3) str)
          _ -> Unknown str

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)


insert :: LogMessage -> MessageTree -> MessageTree
insert msg Leaf = Node Leaf msg Leaf
insert msg@(Unknown _) tree = tree
insert msg tree@(Node left log right) | (timestamp msg) <= (timestamp log) = Node (insert msg left) log right
                                      | otherwise = Node left log (insert msg right)
  where timestamp msg@(LogMessage _ t _) = t


build :: [LogMessage] -> MessageTree
build [] = Leaf
build (m : ms) = insert m (build ms)


inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder tree@(Node left log right) = inOrder left ++ (log : inOrder right)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong msgs =takeMsg $ sortByTime (splitByError msgs)
  where sortByTime msgs= inOrder $ build msgs
        splitByError [] = []
        splitByError (m@(LogMessage (Error e) _ _) : ms) | e > 50 = m : splitByError ms
                                                       | otherwise = splitByError ms
        splitByError (_ : ms) = splitByError ms
        takeMsg [] = []
        takeMsg ((LogMessage _ _ m) : ms) = m : takeMsg ms
