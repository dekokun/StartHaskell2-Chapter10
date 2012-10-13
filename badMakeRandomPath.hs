import System.Random
import System.Directory

randomNums :: [Int]
randomNums = take 999998 $ randomRs (1, 100) (mkStdGen 3)

outputFile :: FilePath
outputFile = "randomPath.txt"

listWrite :: String -> [Int] -> IO ()
listWrite _ [] = return ()
listWrite file list = do
    let (x:xs) = list
    appendFile file $ show x ++ "\n"
    listWrite file xs

main :: IO ()
main = do
    removeFile outputFile
    mapM_ (appendFile outputFile . flip (++) "\n" . show) randomNums
    appendFile outputFile "0\n"
