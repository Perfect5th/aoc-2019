import qualified Data.Text as Text
import qualified Data.Map.Lazy as Map

main = do
    rawInput <- readFile "input.txt"
    let parsed = parseInput rawInput
    print $ solve parsed

parseInput input =
    map (map Text.unpack) [Text.splitOn (Text.pack ",") $ Text.pack i | i <- lines input]

solve (wire1:wire2:[]) =
    let wire1Set = walkWire wire1 addToSet Map.empty
     in walkWire wire2 (findIntersections wire1Set) 1000000

walkWire wire f acc =
    let walkWire [] prev acc = acc
        walkWire (corner:corners) prev acc =
            let (newPrev,steps) = getSteps prev corner
                newAcc = f steps acc
             in walkWire corners newPrev newAcc
     in walkWire wire (0,(0,0)) acc

addToSet steps set = foldl (\m x -> Map.insert (snd x) (fst x) m) set steps

findIntersections map1 [] currMin = currMin
findIntersections map1 (step:steps) currMin =
    case Map.lookup (snd step) map1 of
      Just s -> findIntersections map1 steps $ min currMin (fst step + s)
      Nothing -> findIntersections map1 steps currMin

getSteps (count,(x,y)) (dir:distStr)
  | dir == 'U' = ((count+dist,(x,y + dist)), zip [newCount..newCount+dist] $ zip (repeat x) [y+1..y+dist])
  | dir == 'D' = ((count+dist,(x,y - dist)), zip [newCount..newCount+dist] $ zip (repeat x) [y-1,y-2..y-dist])
  | dir == 'R' = ((count+dist,(x + dist,y)), zip [newCount..newCount+dist] $ zip [x+1..x+dist] $ repeat y)
  | dir == 'L' = ((count+dist,(x - dist,y)), zip [newCount..newCount+dist] $ zip [x-1,x-2..x-dist] $ repeat y)
  where dist = read distStr :: Int
        newCount = count + 1
