import Commands
import DataStructure
import FileManagement

import Control.Monad
import System.IO
import System.Directory

help :: String
help = "HELP -- (c) dakuten\n\n\
\[ ] -- NotDone\n\
\[~] -- InProgress\n\
\[V] -- Done\n\
\\n\
\Available commands:\n\
\add   string -- add a task entitled string\n\
\delete     n -- delete the nth task (starts with 0)\n\
\rename   n s -- rename the nth task s (starts with 0)\n\
\mark n state -- set the nth task Done/NotDone/InProgress\n\
\help         -- show this\n\
\open    path -- open the taskList at path\n\
\save    path -- save the taskList at path\n\
\quit/exit    -- leave\n"

main = do 
  hdir <- getHomeDirectory
  let defaultPath = hdir ++ "/.taskList"
  tl <- readTLIfExists defaultPath
  auxMain defaultPath tl
       
auxMain :: String -> TaskList -> IO ()
auxMain path tl = do
          putStrLn $ showTaskList tl
          putStr "x: "; hFlush stdout
          userInput <- getLine
          case analyseInput userInput of
            Exit         -> do writeTaskList path tl; return ()
            Help         -> do putStrLn help; auxMain path tl
            (Mark i ts)  -> auxMain path (changeTState tl i ts)
            (Rename i n) -> auxMain path (renameTask   tl i n )
            (Delete i  ) -> auxMain path (take i tl ++ drop (i+1) tl)
            (Add      s) -> auxMain path ((Task s NotDone):tl)
            Error     s  -> do putStrLn s; auxMain path tl
            Open      s  -> do tl <- readTLIfExists s ; auxMain s tl
            Save     []  -> do writeTaskList path tl; auxMain path tl
            Save      s  -> do writeTaskList s tl; auxMain path tl
             

