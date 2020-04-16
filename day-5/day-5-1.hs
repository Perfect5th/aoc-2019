import Data.Array.IArray
import Data.Text.Read
import qualified Data.Text as T

main = do
    rawInput <- readFile "input.txt"
    let input   = map (\i -> read (T.unpack i) :: Int) $ T.splitOn (T.pack ",") $ T.pack rawInput
        asArray :: Array Int Int
        asArray = listArray (0,length input - 1) input
     in print $ head $ run asArray 0 [1] []

run prgm pc input output
    | opcode == 99 = output
    | opcode == 3  = run (prgm // [(arg1, head input)]) (pc + 2) (tail input) output
    | opcode == 4  = run prgm (pc + 2) input $ (prgm ! arg1) : output
    | otherwise    = run (prgm // [(arg3, op opcode arg1 arg2)]) (pc + 4) input output
    where opcodeModes = prgm ! pc
          opcode      = opcodeModes `mod` 100
          arg1        = getArg opcode prgm (opcodeModes `quot` 100 `mod` 10) (pc + 1)
          arg2        = getArg opcode prgm (opcodeModes `quot` 1000 `mod` 10) (pc + 2)
          arg3        = prgm ! (pc + 3)

op 1 = (+)
op 2 = (*)

getArg 3 prgm mode pos = prgm ! pos
getArg 4 prgm mode pos = prgm ! pos
getArg opcode prgm mode pos = if mode == 1 then prgm ! pos else prgm ! (prgm ! pos)
