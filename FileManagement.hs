module FileManagement

(
  readTaskList,
  writeTaskList,
  readTLIfExists
)

where

import DataStructure
import System.IO
import Text.Regex
import System.Directory

readTaskList :: FilePath -> IO TaskList
readTaskList path = do
  file <- readFile path
  return $ map analyseLine $ lines file
    where
      analyseLine :: String -> Task
      analyseLine line = let rx = mkRegexWithOpts "^\\[([ ~V])\\] -- (.*)$" True True
                             getTS :: String -> TaskState
                             getTS  u  = case u of 
                                            "V" -> Done
                                            " " -> NotDone
                                            "~" -> InProgress
                                            otherwise -> error $ "Unexpected : '"++u++"'"
                                            
                         in
                         case matchRegex rx line of
                         Just [status, caption] -> Task caption (getTS status)
                         otherwise              -> error $ "Unexpected line in file : "++path

writeTaskList :: FilePath -> TaskList -> IO ()
writeTaskList path tl = writeFile path $ unlines $ map show tl

readTLIfExists :: FilePath -> IO TaskList
readTLIfExists path = do
  exists <- doesFileExist path
  if exists
    then readTaskList path
    else return []
