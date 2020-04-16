import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import Debug.Trace

main = do
    rawInput <- readFile "input.txt"
    let input = lines rawInput
     in print $ solve input

solve orbList =
    let parMap = foldl (\m o -> addToParMap m $ splitOrb o) Map.empty orbList
        orbMap = foldl (\m o -> addToOrbMap m $ splitOrb o) Map.empty orbList
     in traverseOrbs parMap orbMap

splitOrb orb =
    let (parBrkt,child) = splitAt 4 orb
     in (take 3 parBrkt,child)

addToParMap parMap (parent,child) = Map.insert child parent parMap

addToOrbMap orbMap (parent,child) =
    case Map.lookup parent orbMap of
      Just cs -> Map.insert parent (child : cs) orbMap
      Nothing -> Map.insert parent [child] orbMap

traverseOrbs parMap orbMap =
    let traverseUp curr otherCurr currSet otherSet =
            case Map.lookup curr parMap of
              Nothing -> -1
              Just pa ->
                  if Set.member pa otherSet
                     then traverseDown pa otherSet $ length currSet
                     else traverseUp otherCurr pa otherSet (Set.insert pa currSet)
        traverseDown curr orbSet count =
            case Map.lookup curr orbMap of
              Nothing -> -1
              Just cs ->
                  if any (\c -> elem c ["SAN","YOU"]) cs
                     then count
                     else let next = head $ filter (flip Set.member orbSet) cs
                           in traverseDown next orbSet $ count + 1
     in traverseUp "SAN" "YOU" Set.empty Set.empty
