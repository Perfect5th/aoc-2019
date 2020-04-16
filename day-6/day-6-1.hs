import qualified Data.Map.Lazy as Map
import Debug.Trace

main = do
    rawInput <- readFile "input.txt"
    let input = lines rawInput
     in print $ solve input

solve orbList =
     let orbMap = foldl addToMap Map.empty orbList
     in traverseOrbs orbMap

addToMap orbMap orb =
    let (keyBrkt,val) = splitAt 4 orb
        key = take 3 keyBrkt
     in case Map.lookup key orbMap of
          Just v -> Map.insert key (val : v) orbMap
          Nothing -> Map.insert key [val] orbMap

traverseOrbs orbMap =
    let traverseOrbs depth k =
            case Map.lookup k orbMap of
              Just v -> (+) depth $ sum $ map (traverseOrbs $ depth + 1) v
              Nothing -> depth
     in traverseOrbs 0 "COM"
