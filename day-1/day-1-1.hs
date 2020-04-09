-- See https://wiki.mitchellburton.ca/haskell/aoc-intro/day-1-1/ for writeup

main = do
    rawInput <- readFile "input.txt"
    let input = [read i :: Int | i <- lines rawInput] in
        print $ sum [max 0 $ quot i 3 - 2 | i <- input]
