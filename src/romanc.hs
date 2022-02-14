import Roman
import System.Environment

isDec "" = True
isDec (x:xs) = elem x ['0'..'9'] && isDec xs

help progName = do
    putStrLn $ "Usage:"
    putStrLn $ "  " ++ progName ++ " <input>"
    putStrLn $ "  " ++ progName ++ " [-h | --help]"

operateArgs progName args
    | length args /= 1 || elem a ["-h", "--help"] = help progName
    | isDec a = putStrLn $ decToRom (read a :: Integer)
    | otherwise = print $ romToDec a
    where a = args !! 0

main = do
    progName <- getProgName
    args <- getArgs
    operateArgs progName args 
