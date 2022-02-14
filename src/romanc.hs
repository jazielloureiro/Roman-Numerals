import Roman
import System.Environment

isDec "" = True
isDec (x:xs) = elem x ['0'..'9'] && isDec xs

help progName = do
    putStrLn $ "Usage:"
    putStrLn $ "  " ++ progName ++ " <input>"
    putStrLn $ "  " ++ progName ++ " [-h | --help]"
