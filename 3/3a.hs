import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Data.List.Split

main = do
    textLines <- fmap Text.lines (Text.readFile "input")
    let stringLines = map Text.unpack textLines
        out = map (map stringToDirectionList) $ map (splitOn ",") stringLines
    putStrLn $ show out

stringToInt s = read s::Int

stringToDirectionList str = (head str, stringToInt $ tail str)
