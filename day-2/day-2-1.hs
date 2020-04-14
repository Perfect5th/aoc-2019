import Data.Array.IArray
import Data.Text.Read
import qualified Data.Text as T

main = do
    rawInput <- readFile "input.txt"
    let input = parseInput $ T.splitOn (T.pack ",") $ T.pack rawInput
        asArray :: Array Int Int
        asArray = listArray (0, length input - 1) input
     in print $ run $ asArray // [(1, 12), (2, 2)]

parseInput [] = []
parseInput (x:xs) =
    either (\pr -> parseInput xs) (\pr -> fst pr : parseInput xs) $ signed decimal x

run prgm =
    let run pc prgm
         | opcode == 99 = prgm
         | otherwise    = run (pc + 4) $ prgm // [(arg3, op opcode arg1 arg2)]
         where opcode = prgm ! pc
               arg1   = prgm ! (prgm ! (pc + 1))
               arg2   = prgm ! (prgm ! (pc + 2))
               arg3   = prgm ! (pc + 3)
     in run 0 prgm ! 0

op 1 = (+)
op 2 = (*)
