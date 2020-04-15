import qualified Data.Text as Text
import qualified Data.Set as Set

main = do
    rawInput <- readFile "input.txt"
    print $ solve $ map parseWire $ lines rawInput

parseWire rawWire =
    map Text.unpack $ Text.splitOn (Text.pack ",") $ Text.pack rawWire

solve (wire1:wire2:[]) =
    let wireSet = walkWire wire1 addSteps Set.empty
        intersections = walkWire wire2 (findIntersections wireSet) Set.empty
     in minimum $ Set.map (manhattan (0,0)) intersections

walkWire :: [String] -> ([(Int, Int)] -> t1 -> t1) -> t1 -> t1
walkWire wire f acc =
    let walkWire [] prev acc = acc
        walkWire (w:ws) prev acc =
            let (newPrev,steps) = getSteps prev w
                newAcc = f steps acc
             in walkWire ws newPrev newAcc
    in walkWire wire (0,0) acc

addSteps :: [(Int, Int)] -> Set.Set (Int, Int) -> Set.Set (Int, Int)
addSteps steps wireSet = foldl (\ws s -> Set.insert s ws) wireSet steps

findIntersections :: Set.Set (Int, Int) -> [(Int, Int)] -> Set.Set (Int, Int) -> Set.Set (Int, Int)
findIntersections wire1Set steps wire2Set =
    let addIfPresent w2s s = if Set.member s wire1Set then Set.insert s w2s else w2s
     in foldl addIfPresent wire2Set steps

getSteps (prevX,prevY) (dir:distStr)
  | dir == 'U' = ((prevX,prevY + dist),zip (repeat prevX) [prevY + 1..prevY + dist])
  | dir == 'R' = ((prevX + dist,prevY),zip [prevX + 1..prevX + dist] (repeat prevY))
  | dir == 'D' = ((prevX,prevY - dist),zip (repeat prevX) [prevY - 1,prevY - 2..prevY - dist])
  | dir == 'L' = ((prevX - dist,prevY),zip [prevX - 1,prevX - 2..prevX - dist] (repeat prevY))
  where dist = read distStr :: Int

manhattan (startX,startY) (endX,endY) = abs (endX - startX) + abs (endY - startY)
