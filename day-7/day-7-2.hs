import Data.Array.IArray
import Data.Text.Read
import qualified Data.List as L
import qualified Data.Text as T

type NextCycle = (Int -> CycleState)
data CycleState = Input NextCycle | Output Int NextCycle | Halted

main = do
    rawInput <- readFile "input.txt"
    let input   = map (\i -> read (T.unpack i) :: Int) $ T.splitOn (T.pack ",") $ T.pack rawInput
        asArray = listArray (0,length input - 1) input
        allSettings = generateSettings
     in print $ maximum $ map (runPipelined asArray) allSettings

generateSettings =
    let r = [5..9]
     in filter ((==5) . length . L.nub) [[a,b,c,d,e] | a <- r, b <- r, c <- r, d <- r, e <- r]

runPipelined prgm settings =
    let run [] output = head output
        run (amp:amps) output =
            case amp of
              Input n -> run (n (head output) : amps) $ tail output
              Output i n -> run (amps ++ [n 0]) $ output ++ [i]
              Halted -> run amps output
        prepareAmps setting =
            case runCycle prgm of
              Input n -> n setting
              Output i n -> error "Got output when preparing amp"
              Halted -> error "Amp halted during prep"
     in run (map prepareAmps settings) [0]

runCycle prgm =
    let run :: Array Int Int -> Int -> CycleState
        run prgm pc
            | opcode == 99 = Halted
            | opcode == 3  = Input (\i -> run (prgm // [(arg1, i)]) (pc + 2))
            | opcode == 4  = Output (prgm ! arg1) (\_ -> run prgm $ pc + 2)
            | opcode == 5  = run prgm (if arg1 /= 0 then arg2 else pc + 3)
            | opcode == 6  = run prgm (if arg1 == 0 then arg2 else pc + 3)
            | otherwise    = run (prgm // [(arg3, op opcode arg1 arg2)]) $ pc + 4
            where opcodeModes = prgm ! pc
                  opcode      = opcodeModes `mod` 100
                  arg1        = getArg opcode prgm (opcodeModes `quot` 100 `mod` 10) (pc + 1)
                  arg2        = getArg opcode prgm (opcodeModes `quot` 1000 `mod` 10) (pc + 2)
                  arg3        = prgm ! (pc + 3)
     in run prgm 0

op 1 = (+)
op 2 = (*)
op 7 = (\arg1 arg2 -> if arg1 < arg2 then 1 else 0)
op 8 = (\arg1 arg2 -> if arg1 == arg2 then 1 else 0)

getArg 3 prgm mode pos = prgm ! pos
getArg 4 prgm mode pos = prgm ! pos
getArg opcode prgm mode pos = if mode == 1 then prgm ! pos else prgm ! (prgm ! pos)
