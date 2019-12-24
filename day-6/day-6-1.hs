import qualified Data.Map as Map

main = do
    contents <- readFile "day-6/input.txt"
    print . solve $ lines contents

solve ipt = checksum $ parse ipt

parse ipt = foldr addToMap Map.empty ipt

addToMap orb orbMap =
    let (p, child) = splitAt 4 orb
        parent = take 3 p
    in  case Map.lookup parent orbMap of
        Just cs -> Map.insert parent (child : cs) orbMap
        Nothing -> Map.insert parent [child] orbMap

checksum orbMap =
    let helper m depth k =
            case Map.lookup k m of
                Nothing -> depth
                Just cs -> (depth +) . sum $ map (helper m (depth + 1)) cs
    in   helper orbMap 0 "COM"
