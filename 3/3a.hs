import Data.List.Split
import qualified Data.List    as List
import qualified Data.Set     as Set
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

main = do
    textLines <- fmap Text.lines (Text.readFile "input.txt")
    let stringLines = map Text.unpack textLines
        directions = map (map stringToDirectionList) $ map (splitOn ",") stringLines
        allPoints = map getPoints directions
        commonPoints = Set.delete (0,0) (foldl1 Set.intersection allPoints)
        result = head $ List.sort $ map md $ Set.toList commonPoints

    putStrLn $ show result

stringToInt s = read s::Int

stringToDirectionList str = (head str, stringToInt $ tail str)

pointsOnPartialConnection (startx, starty) (direction, length)
    | direction == 'L' = ((startx - length, starty), zip [startx - length .. startx] $ repeat starty)
    | direction == 'R' = ((startx + length, starty), zip [startx .. startx + length] $ repeat starty)
    | direction == 'U' = ((startx, starty + length), zip (repeat startx) [starty .. starty + length])
    | otherwise        = ((startx, starty - length), zip (repeat startx) [starty - length  .. starty])

getPoints' (x,y) pointsList [] = Set.fromList pointsList
getPoints' (x,y) pointsList (a:as) = getPoints' newstart newPointsList as
    where 
    (newstart, extraPoints) = pointsOnPartialConnection (x,y) a
    newPointsList = pointsList ++ extraPoints

getPoints = getPoints' (0,0) []

md (x,y) = (abs x) + (abs y)



