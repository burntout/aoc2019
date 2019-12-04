import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

main = do
    tls <- fmap Text.lines (Text.readFile "input.txt")
    let ss = map Text.unpack tls
        ints = map (massToFuel . stringToInt) ss
    putStrLn $ show $ sum ints

stringToInt s = read s::Int

massToFuel m = sum $ tail $ takeWhile (>0) $ iterate massToFuel' m

massToFuel' m 
    | (m `div` 3) - 2 > 0 = (m `div` 3) - 2
    | otherwise           = 0


