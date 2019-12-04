import Data.List.Split
import qualified Data.List    as List
import qualified Data.Set     as Set
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

main = do
    textLines <- fmap Text.lines (Text.readFile "input.txt")
    let stringLines = map Text.unpack textLines
        directions = parseInput stringLines
        allPoints = map getPoints directions
        setAllPoints = map Set.fromList allPoints
        commonPoints = Set.toList $ Set.delete (0,0) $ foldl1 Set.intersection setAllPoints
        steps = map (getSteps commonPoints) allPoints 
        result = head $ List.sort $ zipWith (+) (head steps) (last steps)

    putStrLn $ show result

stringToInt s = read s::Int

stringToDirectionList str = (head str, stringToInt $ tail str)

parseInput inputLines = map (map stringToDirectionList) $ map (splitOn ",") inputLines


-- lots of drama here because it matters which way we order things, 
-- and that we don't double count starts and ends.
pointsOnPartialConnection (startx, starty) (direction, length)
    | direction == 'L' = ((startx - length, starty), tail $ reverse $ zip [startx - length .. startx] $ repeat starty)
    | direction == 'R' = ((startx + length, starty), tail $ zip [startx .. startx + length] $ repeat starty)
    | direction == 'U' = ((startx, starty + length), tail $ zip (repeat startx) [starty .. starty + length])
    | otherwise        = ((startx, starty - length), tail $ reverse $ zip (repeat startx) [starty - length  .. starty])

getPoints' (x,y) pointsList [] = pointsList
getPoints' (x,y) pointsList (a:as) = getPoints' newstart newPointsList as
    where 
    (newstart, extraPoints) = pointsOnPartialConnection (x,y) a
    newPointsList = pointsList ++ extraPoints

getPoints = getPoints' (0,0) []

md (x,y) = (abs x) + (abs y)

unsafeIndex x xs = n 
    where
    Just n = List.elemIndex x xs

getSteps [] _ = []
getSteps (a:as) l = (1 + (unsafeIndex a l) : getSteps as l)



-- Test Data
testData = ["R8,U5,L5,D3","U7,R6,D4,L4"]
allPoints = map getPoints $ parseInput testData
commonPoints = Set.toList $ Set.delete (0,0) $ foldl1 Set.intersection $ map Set.fromList $ map getPoints $ parseInput testData
steps = map (getSteps commonPoints) allPoints
