import System.IO
import System.Directory
import Data.List

main = do
    handle <- openFile "./auxiliary/todo.txt" ReadMode
    (tempName, tempHandle) <- openTempFile "auxiliary" "temp"
    contents <- hGetContents handle
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStrLn "These are your TO-DO items:"
    putStr $ unlines numberedTasks
    putStrLn "Which one do you want to delete?"
    numberString <- getLine
    let number = read numberString
        newTodoItems = delete (todoTasks !! number) todoTasks
    hPutStr tempHandle $ unlines newTodoItems
    hClose handle
    hClose tempHandle
    removeFile "./auxiliary/todo.txt"
    renameFile tempName "./auxiliary/todo.txt"
