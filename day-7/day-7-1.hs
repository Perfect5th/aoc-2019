import Data.Array.IArray
import Data.Text.Read
import qualified Data.List as L
import qualified Data.Text as T

main = do
    rawInput <- readFile "input.txt"
    let input   = map (\i -> read (T.unpack i) :: Int) $ T.splitOn (T.pack ",") $ T.pack rawInput
        asArray :: Array Int Int
        asArray = listArray (0,length input - 1) input
        allSettings = generateSettings
     in print $ maximum $ map (head . runWithSettings asArray) allSettings

generateSettings =
    let r = [0..4]
     in filter ((==5) . length . L.nub) [[a,b,c,d,e] | a <- r, b <- r, c <- r, d <- r, e <- r]

runWithSettings prgm settings =
    let runWithSettings [] output = output
        runWithSettings (s:ss) output =
            runWithSettings ss $ run prgm 0 [] (s : output)
     in runWithSettings settings [0]

run prgm pc output input
    | opcode == 99 = output
    | opcode == 3  = run (prgm // [(arg1, head input)]) (pc + 2) output (tail input)
    | opcode == 4  = run prgm (pc + 2) ((prgm ! arg1) : output) input
    | opcode == 5  = run prgm (if arg1 /= 0 then arg2 else pc + 3) output input
    | opcode == 6  = run prgm (if arg1 == 0 then arg2 else pc + 3) output input
    | otherwise    = run (prgm // [(arg3, op opcode arg1 arg2)]) (pc + 4) output input
    where opcodeModes = prgm ! pc
          opcode      = opcodeModes `mod` 100
          arg1        = getArg opcode prgm (opcodeModes `quot` 100 `mod` 10) (pc + 1)
          arg2        = getArg opcode prgm (opcodeModes `quot` 1000 `mod` 10) (pc + 2)
          arg3        = prgm ! (pc + 3)

op 1 = (+)
op 2 = (*)
op 7 = (\arg1 arg2 -> if arg1 < arg2 then 1 else 0)
op 8 = (\arg1 arg2 -> if arg1 == arg2 then 1 else 0)

getArg 3 prgm mode pos = prgm ! pos
getArg 4 prgm mode pos = prgm ! pos
getArg opcode prgm mode pos = if mode == 1 then prgm ! pos else prgm ! (prgm ! pos)
