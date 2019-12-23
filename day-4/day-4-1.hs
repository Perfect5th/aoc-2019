import qualified Data.Char as Char

minval = 264360
maxval = 746325

is_ascending :: [Int] -> Bool
is_ascending []             = True
is_ascending (x1 : [])      = True
is_ascending (x1 : x2 : xs) = x1 <= x2 && (is_ascending $ x2 : xs)

has_double :: [Int] -> Bool
has_double []             = False
has_double (x1 : [])      = False
has_double (x1 : x2 : xs) = x1 == x2 || (has_double $ x2 : xs)

expand :: String -> [Int]
expand str = map Char.digitToInt str

valid :: Integer -> Bool
valid pw =
    let expanded = expand $ show pw
    in  has_double expanded && is_ascending expanded

solve :: Integer -> Integer -> Int
solve m0 m1 = length $ filter valid [m0..m1]

main = print $ solve minval maxval
