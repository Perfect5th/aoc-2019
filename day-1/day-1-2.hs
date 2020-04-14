-- See https://wiki.mitchellburton.ca/haskell/aoc-intro/day-1-2/ for writeup

main = do
    rawInput <- readFile "input.txt"
    let input = [read i :: Int | i <- lines rawInput] in
        print $ sum $ map fuelRequired input

fuelRequired x
    | fuel <= 0 = 0
    | otherwise = fuel + fuelRequired fuel
    where fuel = quot x 3 - 2
