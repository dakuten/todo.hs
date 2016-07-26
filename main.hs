import Commands
import DataStructure

import Control.Monad


main = do auxMain []
       
auxMain :: TaskList -> IO ()
auxMain tl = do
          putStrLn $ showTaskList tl
          putStr "x: "
          userInput <- getLine
          case analyseInput userInput of
            Exit         -> do return ()
            Help         -> do putStrLn "This is some help."; auxMain tl
            (Mark i ts)  -> do auxMain (changeTState tl i ts)
            (Rename i n) -> do auxMain (renameTask   tl i n )
            (Delete i  ) -> do auxMain (take i tl ++ drop (i+1) tl)
            (Add      s) -> do auxMain ((Task s NotDone):tl)
            
             

