import qualified Data.List as List

inOp = 3
outOp = 4
jiftOp = 5
jiffOp = 6
haltOp = 99

data CompState = CompState { position :: Int
                           , tape :: [Int]
                           , input :: [Int]
                           , output :: [Int]
                           , halted :: Bool
                           , op :: Int
                           , vals :: (Int, Int, Int)
                           , modes :: (Int, Int, Int)
                           , valP :: Int
                           }

main = do
    input <- readFile "day-7/input.txt"
    print . solve $ prep input

prep s = case dropWhile (==',') s of
             "" -> []
             s' -> (read w::Int) : prep s''
                   where (w, s'') = break (==',') s'

solve tape =
    let allSettings = generateSettings
    in  maximum $ map (applyInput tape) allSettings

generateSettings =
    filter ((==5) . length . List.nub) [[x, y, z, w, v] | x <- [0..4], y <- [0..4], z <- [0..4], w <- [0..4], v <- [0..4]]

applyInput tape settings = foldr (runComps tape) 0 settings

runComps tape setting input =
    let state = CompState { position=0
                          , tape=tape
                          , input=[setting, input]
                          , output=[]
                          , halted=False
                          , op=0
                          , vals=(0, 0, 0)
                          , modes=(0, 0, 0)
                          , valP=0 }

    in  runCycle state

runCycle state =
    let cycled = cycleState state
    in  if halted cycled
            then head $ output cycled
            else runCycle cycled

runNCycles 0 state = head $ output state
runNCycles n state =
    let cycled = cycleState state
    in  if halted cycled
            then head $ output cycled
            else runNCycles (n - 1) cycled

cycleState = execute . decode . fetch

fetch state =
    let opcode = tape state !! position state
    in  state { op=mod opcode 100
              , modes=( mod (div opcode 100) 10
                      , mod (div opcode 1000) 10
                      , mod (div opcode 10000) 10
                      )
              }

decode state = state { vals= getVals state, valP=getValP state }

getVals state =
    let (mode1, mode2, _) = modes state
    in  ( getVal 1 mode1 state
        , getVal 2 mode2 state
        , getVal 3     1 state
        )

getVal pos mode state
    | op state == haltOp = 0
    | pos > 1 && (op state == inOp || op state == outOp) = 0
    | mode == 1 || op state == inOp = tape state !! (position state + pos)
    | otherwise = tape state !! (tape state !! (position state + pos))

getValP state
    | op state == inOp || op state == outOp = 2
    | op state == haltOp = 1
    | op state == jiftOp || op state == jiffOp = 3
    | otherwise = 4

execute state =
    let (val1, val2, val3) = vals state
    in  (getFunc state) val1 val2 val3

getFunc state =
    case op state of
         1  -> calcFunc (+) state
         2  -> calcFunc (*) state
         3  -> getInput state
         4  -> writeOutput state
         5  -> jumpFunc (/=0) state
         6  -> jumpFunc (==0) state
         7  -> calcFunc (\x y -> if x < y then 1 else 0) state
         8  -> calcFunc (\x y -> if x == y then 1 else 0) state
         99 -> haltFunc state
         _ -> (\x y z -> state)

calcFunc op state val1 val2 val3 =
    let valE = op val1 val2
    in  writeBack state val3 valE

writeBack state loc val =
    let (tapehead, tapetail) = splitAt loc $ tape state
        newTape = tapehead ++ val : tail tapetail
        newPos = (position state) + (valP state)
    in  state { tape=newTape, position=newPos }

getInput state val1 val2 val3 =
    let (i:newInput) = input state
        newState = state { input=newInput }
    in  writeBack newState val1 i

writeOutput state val1 val2 val3 =
    let newOutput = val1 : output state
        newPos = (position state) + (valP state)
    in  state { output=newOutput, position=newPos }

jumpFunc op state val1 val2 val3 =
    let (val1, val2, _) = vals state
        newPos = if op val1 then val2 else (position state) + (valP state)
    in  state { position=newPos }

haltFunc state val1 val2 val3 =
    state { position=(position state) + (valP state)
          , halted=True
          }
