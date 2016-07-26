module DataStructure

(
  TaskState(..), 
  TaskName,
  Task(..),
  TaskList,
  showTaskList,
  sortTaskList,
  renameTask,
  changeTState
)

where


data TaskState = NotDone | InProgress | Done deriving (Read, Eq, Ord, Bounded)
type TaskName  = String
data Task      = Task TaskName TaskState
type TaskList  = [Task]

instance Show TaskState where
  show NotDone    = "[ ]"
  show InProgress = "[~]"
  show Done       = "[V]"
	

instance Show Task where
  show (Task name state) = show state ++ " -- " ++ name 
 

showTaskList :: TaskList -> String
showTaskList []     = "Nothing."
showTaskList [t]    = show t 
showTaskList (t:ts) = show t ++ "\n" ++ showTaskList ts 

sortTaskList :: TaskList -> TaskList
sortTaskList tl = [t | t@(Task _ state)<-tl, state == NotDone] 
		++[t | t@(Task _ state)<-tl, state == InProgress]
		++[t | t@(Task _ state)<-tl, state == Done]

renameTask :: TaskList -> Int -> TaskName -> TaskList
renameTask ((Task _ state):ts) 0 name = (Task name state):ts
renameTask (t:ts) i name              = t:(renameTask ts (i-1) name)
renameTask [] _ _ = error "La liste est vide."

changeTState :: TaskList -> Int -> TaskState -> TaskList
changeTState ((Task name _):ts) 0 state = (Task name state):ts
changeTState (t:ts) i state             = t:(changeTState ts (i-1) state)
changeTState [] _ _ = error "La liste est vide."

