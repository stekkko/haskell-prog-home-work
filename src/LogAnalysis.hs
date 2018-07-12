{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage string = case words string !! 0 of
                      "I" -> LogMessage Info 
                                        (read $ words string !! 1) 
                                        (unwords $ (drop 2) $ words string)
                      "W" -> LogMessage Warning 
                                        (read $ words string !! 1)
                                        (unwords $ (drop 2) $ words string)
                      "E" -> LogMessage (Error (read $ words string !! 1)) 
                                        (read $ words string !! 2) 
                                        (unwords $ (drop 3) $ words string)
                      _   -> Unknown string

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

{-
insert :: LogMessage -> Int
insert logMes = case logMes of 
                Unknown _ -> 0
                LogMessage _ _ _ -> 1
-}

insert :: LogMessage -> MessageTree -> MessageTree
insert logMes mesTree = case (logMes, mesTree) of
                        (_ , Leaf) -> Node Leaf logMes Leaf
                        (LogMessage _ ts _, Node left n@(LogMessage _ tv _) right) 
                                   -> if ts > tv then Node left n (insert logMes right) else Node (insert logMes left) n right
                        _          -> mesTree

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x (build xs)

inOrder :: MessageTree -> [LogMessage]
inOrder mesTree = case mesTree of 
                  Leaf                -> []
                  Node left log right -> (inOrder left) ++ [log] ++ (inOrder right) 

createReport :: [LogMessage] -> [String]
createReport [] = []
createReport (x:xs) = case x of
                           LogMessage (Error ts) _ inf -> if ts >= 50 then inf : createReport xs else createReport xs
                           _ -> createReport xs

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = (createReport . inOrder . build)
