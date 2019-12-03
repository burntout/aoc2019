import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import qualified Data.Sequence as S
import Data.List.Split

main = do
    tls <- fmap Text.lines (Text.readFile "input")
    let ss = map stringToInt $ head $ map (splitOn "," . Text.unpack) tls
        seq = S.fromList ss
        result = program $ initialise seq 1202
    putStrLn $ show $ result

stringToInt s = read s::Int

program seq = program' (seq, 0)

program' (seq, cnt)  
    | S.index seq cnt == 1 = program' $ add seq cnt
    | S.index seq cnt == 2 = program' $ mul seq cnt
    | otherwise            = seq `S.index` 0 

add seq cnt = (S.update updatedIndex (val1 + val2) seq, cnt + 4)
    where 
    val1 = S.index seq $ S.index seq (cnt + 1)
    val2 = S.index seq $ S.index seq (cnt + 2)
    updatedIndex = S.index seq (cnt + 3)


mul seq cnt = (S.update updatedIndex (val1 * val2) seq, cnt + 4)
    where 
    val1 = S.index seq $ S.index seq (cnt + 1)
    val2 = S.index seq $ S.index seq (cnt + 2)
    updatedIndex = S.index seq (cnt + 3)

initialise seq n = newSeq
    where 
    (x,y) = n `divMod` 100
    s1 = S.update 1 x seq 
    newSeq = S.update 2 y s1
