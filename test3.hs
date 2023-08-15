import Prelude
import System.Directory (listDirectory, doesFileExist, doesDirectoryExist)

listFilesRecursively :: FilePath -> FilePath

listFilesRecursively dir = do 
    contents <- listDirectory dir 
    let paths = map (\f -> dir ++ "/" ++ f) contents
    files <- filter doesFileExist paths
    directories <- filter doesDirectoryExist paths
    nestedFiles <- concat <$> map listFilesRecursively directories
    return $ files ++ nestedFiles

main :: IO ()
main = do
    putStrLn "Enter Director path: "
    dirPath <- getLine
    files <- listFilesRecursively dirPath
    putStrLn "List all files in the directory and its subdirectories: "
    mapM_ putStrLn files