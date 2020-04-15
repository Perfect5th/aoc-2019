import qualified Data.Text as Text
import qualified Data.Set as Set
import Debug.Trace

main = do
    rawInput <- readFile "input.txt"
    let parsed = parseInput rawInput
    print $ solve parsed

parseInput input =
    map (map Text.unpack) [Text.splitOn (Text.pack ",") $ Text.pack i | i <- lines input]

solve (wire1:wire2:[]) =
    let wire1Set = walkWire wire1 addToSet Set.empty
        intersections = walkWire wire2 (findIntersections wire1Set) Set.empty
     in minimum $ Set.map manhattan intersections

walkWire :: [String] -> ([(Int, Int)] -> t1 -> t1) -> t1 -> t1
walkWire wire f acc =
    let walkWire [] prev acc = acc
        walkWire (corner:corners) prev acc =
            let (newPrev,steps) = getSteps prev corner
                newAcc = f steps acc
             in walkWire corners newPrev newAcc
     in walkWire wire (0,0) acc

addToSet :: [(Int, Int)] -> Set.Set (Int, Int) -> Set.Set (Int, Int)
addToSet steps set = foldl (\s x -> Set.insert x s) set steps

findIntersections :: Set.Set (Int, Int) -> [(Int, Int)] -> Set.Set (Int, Int) -> Set.Set (Int, Int)
findIntersections set1 [] set2 = set2
findIntersections set1 (step:steps) set2 =
    if Set.member step set1
       then Set.insert step $ findIntersections set1 steps set2
       else findIntersections set1 steps set2

manhattan (x,y) = abs x + abs y

getSteps (x,y) (dir:distStr)
  | dir == 'U' = ((x,y + dist), zip (repeat x) [y+1..y+dist])
  | dir == 'D' = ((x,y - dist), zip (repeat x) [y-1,y-2..y-dist])
  | dir == 'R' = ((x + dist,y), zip [x+1..x+dist] $ repeat y)
  | dir == 'L' = ((x - dist,y), zip [x-1,x-2..x-dist] $ repeat y)
  where dist = read distStr :: Int
