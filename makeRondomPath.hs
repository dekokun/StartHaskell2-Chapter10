import System.Random
import System.Environment

randomNums :: [Int]
randomNums = randomRs (1, 100) (mkStdGen 3)

outputFile :: FilePath
outputFile = "randomPath.txt"

main :: IO ()
main = do
    (count:_) <- getArgs
    let randomNums' = take (read count - 1) randomNums
        contents = concatMap (flip (++) "\n" . show) randomNums'
    writeFile outputFile contents
    appendFile outputFile "0\n"
