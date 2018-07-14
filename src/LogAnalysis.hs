{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage string = case words string of
                      "I":ts:rest    -> LogMessage Info (read ts) (unwords rest)
                      "W":ts:rest    -> LogMessage Warning (read ts) (unwords rest)
                      "E":ns:ts:rest -> LogMessage (Error (read ns)) (read ts) (unwords rest)
                      _              -> Unknown string

parse :: String -> [LogMessage]
parse s = map parseMessage $ lines s

insert :: LogMessage -> MessageTree -> MessageTree
insert logMes mesTree = case (logMes, mesTree) of
                        (Unknown _, _) -> mesTree
                        (_ , Leaf)     -> Node Leaf logMes Leaf
                        (LogMessage _ ts _, Node left n@(LogMessage _ tv _) right) 
                                       -> if ts > tv 
                                          then Node left n (insert logMes right) 
                                          else Node (insert logMes left) n right
                        _              -> mesTree

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder mesTree = case mesTree of 
                  Leaf                -> []
                  Node left lg right -> inOrder left ++ [lg] ++ inOrder right 

createReport :: [LogMessage] -> [String]
createReport [] = []
createReport (x:xs) = case x of
                      LogMessage (Error ts) _ inf -> if ts >= 50 
                                                     then inf : createReport xs 
                                                     else createReport xs
                      _                           -> createReport xs

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = createReport . inOrder . build
