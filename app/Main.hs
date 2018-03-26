module Main where

import           Control.Monad       (when, void)
import           Control.Monad.State
import           Data.Time
import           Data.Map as Mp

data Task = On NominalDiffTime UTCTime | Off NominalDiffTime 
    deriving Show

type Tasks = Mp.Map String Task

main :: IO ()
main = void (runStateT code Mp.empty)
 
code :: StateT Tasks IO ()
code = do
  liftIO $ putStr "\n>> "
  inp <- liftIO getLine
  let (cmd : args) = words inp
  process cmd args
  when (cmd /= "quit" && cmd /= "q") code

process :: [Char] -> [[Char]] -> StateT Tasks IO ()
process cmd args
  | cmd == "?" || cmd == "help"  = help
  | cmd == "+" || cmd == "start" = startTask name
  | cmd == "-" || cmd == "stop"  = stopTask name
  | cmd == "*" || cmd == "show"  = showTask name
  | cmd == "l" || cmd == "list"  = listTask
  | cmd == "q" || cmd == "quit"  = return ()
  | cmd == "d"                   = debug
  | otherwise                    = liftIO $ putStrLn "~~~ Invalid command, use help or ?"
  where
    name = case args of
      nm : _ -> nm
      _      -> "default"

help :: StateT Tasks IO ()
help = liftIO $ do 
  putStrLn "---------------------------"
  putStrLn "Available commands"
  putStrLn "start [task-name]"
  putStrLn "stop  [task-name]"
  putStrLn "show  [task-name]"
  putStrLn "list"
  putStrLn "help"
  putStrLn "quit"
  putStrLn "---------------------------"
  putStrLn "Available abbreviations:"
  putStrLn "start: +"
  putStrLn "stop:  -"
  putStrLn "show:  *"
  putStrLn "list:  l"
  putStrLn "help:  ?"
  putStrLn "quit:  q"
  putStrLn "---------------------------"

startTask :: String -> StateT Tasks IO ()
startTask name = do
  m <- get
  let t = Mp.lookup name m
  (diff, st) <- liftIO $ currentState t
  tmNow <- liftIO getCurrentTime
  put $ Mp.insert name (On diff tmNow) m
  liftIO $ putStrLn $ "Task <" ++ name ++ "> " ++ status st ++ "started"
  where
    status 0 = "added and "
    status 1 = ""
    status 2 = "was already "

stopTask :: String -> StateT Tasks IO ()
stopTask name = do
  m <- get
  let t = Mp.lookup name m
  (diff, st) <- liftIO $ currentState t
  put $ Mp.insert name (Off diff) m
  liftIO $ putStrLn $ "Task <" ++ name ++ "> " ++ status st ++ "stopped"
  where
    status 0 = "added and "
    status 1 = "was already "
    status 2 = ""

showTask :: String -> StateT Tasks IO ()
showTask name = do
  m <- get
  let t = Mp.lookup name m
  (diff, st) <- liftIO $ currentState t
  liftIO $ putStrLn $ "Task <" ++ name ++ "> " ++ status st
  liftIO $ putStrLn $ "Current time for task <" ++ name ++ ">: " ++ diffFormat' diff
  where
    status 0 = "not exist"
    status 1 = "stopped"
    status 2 = "started"

listTask :: StateT Tasks IO ()
listTask = do
  m <- get
  liftIO $ putStrLn "---------------------------------------------------------------"
  liftIO $ putStrLn "Tasks:"
  mapM_ listLine $ Mp.toList m
  liftIO $ putStrLn "---------------------------------------------------------------"
  where
    listLine (name, task) = liftIO $ do
      (diff, st) <- currentState $ Just task
      putStrLn $ "<" ++ name ++ ">: " ++ status st ++ ", current time: " ++ diffFormat' diff
    status 1 = "stopped"
    status 2 = "started"

currentState :: Num t => Maybe Task -> IO (NominalDiffTime, t)
currentState (Just (On old tmStart)) = do
  tmNow <- getCurrentTime
  return (diffUTCTime (addUTCTime old tmNow) tmStart, 2)
currentState (Just (Off old)) = return (old, 1)
currentState Nothing = return (0, 0)

debug :: StateT Tasks IO ()
debug = do
  m <- get
  liftIO $ print m
  liftIO $ print $ Mp.toList m

diffFormat' :: NominalDiffTime -> [Char]
diffFormat' diff = diffFormat diff ++ " (" ++ show diff ++ ")"

diffFormat :: NominalDiffTime -> String
diffFormat x
  | x >= 24*60*60 = show (value x 24*60*60) ++ "d " -- ++ (diffFormat $ rest x 24*60*60)
  | x >= 60*60 = show (value x 60*60) ++ "h " ++ diffFormat (rest x 60*60)
  | x >= 60 = show (value x 60) ++ "m " ++ diffFormat (rest x 60)
  | otherwise = show x

value :: NominalDiffTime -> Integer -> Integer
value x z = floor x `div` z

rest :: NominalDiffTime -> Integer -> NominalDiffTime
rest x z = x - fromInteger (value x z * z) :: NominalDiffTime