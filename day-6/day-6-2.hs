import qualified Data.Map as Map

main = do
    contents <- readFile "day-6/input.txt"
    print . solve $ lines contents

solve ipt = let parsed = parse ipt
                youPath = reverse $ findPath "YOU" parsed
                sanPath = reverse $ findPath "SAN" parsed
            in  commonPath youPath sanPath

parse ipt = foldr addToMap Map.empty ipt

addToMap orb orbMap =
    let (parent, child) = split orb
    in  case Map.lookup parent orbMap of
        Just cs -> Map.insert parent (child : cs) orbMap
        Nothing -> Map.insert parent [child] orbMap

split []       = ([], [])
split (')':xs) = ("", xs)
split ( x :xs) = (x:xs', xs'')
    where (xs', xs'') = split xs

findPath target orbMap =
    let helper m target path k =
            case Map.lookup k m of
                Nothing -> []
                Just cs -> if any (target==) cs
                           then path
                           else let candidates = filter ((0<) . length)
                                                 $ map (\c -> helper m target (c : path) c) cs
                                in  case candidates of
                                        []     -> []
                                        (x:xs) -> x
    in   helper orbMap target [] "COM"

commonPath x y =
    let helper [] _  _           = -1
        helper _  [] _           = -1
        helper (x:x') (y:y') acc
            | x == y    = helper x' y' acc
            | otherwise = length x' + length y' + 2

    in  helper x y 0
