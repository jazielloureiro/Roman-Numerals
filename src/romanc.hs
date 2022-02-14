import Roman
import System.Environment

help progName = do
    putStrLn $ "Usage:"
    putStrLn $ "  " ++ progName ++ " <input>"
    putStrLn $ "  " ++ progName ++ " [-h | --help]"
