import qualified Data.Char as Char
import qualified Data.List as List

minval = 264360
maxval = 746325

is_ascending :: [Int] -> Bool
is_ascending []             = True
is_ascending (x1 : [])      = True
is_ascending (x1 : x2 : xs) = x1 <= x2 && (is_ascending $ x2 : xs)

has_double :: String -> Bool
has_double str = any ((== 2) . length) $ List.group str

expand :: String -> [Int]
expand str = map Char.digitToInt str

valid :: Integer -> Bool
valid pw =
    let expanded = expand $ show pw
    in  (has_double $ show pw) && is_ascending expanded

solve :: Integer -> Integer -> Int
solve m0 m1 = length $ filter valid [m0..m1]

main = print $ solve minval maxval
