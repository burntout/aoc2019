import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

main = do
    tls <- fmap Text.lines (Text.readFile "input.txt")
    let ss = map Text.unpack tls
        ints = map (massToFuel . stringToInt) ss
    putStrLn $ show $ sum ints

stringToInt s = read s::Int
massToFuel m =  (m `div` 3) - 2

