module Commands

(
  Command(..),
  analyseInput
)

where

import DataStructure(TaskState)
import Data.Char(toLower)
import Data.List(intercalate)

data Command = Add String 
             | Delete Int 
             | Rename Int String
             | Mark Int TaskState
             | Help
             | Exit
             deriving (Show)

analyseInput :: String -> Command
analyseInput input = case (map toLower first) of
                      "add"    -> Add $ intercalate " " remain
                      "delete" -> if length remain >= 1 
                                  then
                                    Delete $ parseInt (head remain)
                                  else
                                    error "A number was expected"
                      "rename" -> if length remain >= 2
                                  then
                                    Rename (parseInt (head remain))
                                           (intercalate " " $ tail remain)
                                  else
                                    error "Two arguments where expected"
                      "mark"   -> if length remain >= 2
                                  then let (second:third:_) = remain in
                                    Mark (parseInt second)
                                         (read third  :: TaskState)
                                  else 
                                    error "Two arguments were expected"
                      "help"   -> Help
                      "quit"   -> Exit
                      "exit"   -> Exit
                      _        -> error "Unknown command"
		    where
                      (first:remain) = words input
                      parseInt :: String -> Int
                      parseInt s = read s :: Int

