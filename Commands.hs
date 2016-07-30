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
             | Open String
             | Save String
             | Help
             | Exit
             | Error String deriving (Show)

analyseInput :: String -> Command
analyseInput input = case (map toLower first) of
                      "add"    -> Add $ intercalate " " remain
                      "delete" -> if length remain >= 1 
                                  then
                                    Delete $ parseInt (head remain)
                                  else
                                    Error "A number was expected"
                      "rename" -> if length remain >= 2
                                  then
                                    Rename (parseInt (head remain))
                                           (intercalate " " $ tail remain)
                                  else
                                    Error "Two arguments where expected"
                      "mark"   -> if length remain >= 2
                                  then let (second:third:_) = remain in
                                    Mark (parseInt second)
                                         (read third  :: TaskState)
                                  else 
                                    Error "Two arguments were expected"
                      "help"   -> Help
                      "open"   -> if length remain > 0
                                  then
                                    Open $ intercalate " " remain
                                  else
                                    Error "A path must be provided."

                      "save"   -> if length remain > 0
                                  then
                                    Save $ intercalate " " remain
                                  else
                                    Save ""
                      "quit"   -> Exit
                      "exit"   -> Exit
                      _        -> Error "Unknown command"
		    where
                      (first:remain) = words input
                      parseInt :: String -> Int
                      parseInt s = read s :: Int

