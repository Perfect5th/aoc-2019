import System.IO

inOp = 3
outOp = 4
haltOp = 99

data CompState = CompState { position :: Int
                           , tape :: [Int]
                           , input :: IO String
                           , output :: (String -> IO ())
                           , halted :: Bool
                           , opcode :: Int
                           , op :: Int
                           , vals :: (Int, Int, Int)
                           , modes :: (Int, Int, Int)
                           , valP :: Int
                           }

intCodeComputer :: IO CompState -> IO ()
intCodeComputer state = do
    currState <- state
    if halted currState
        then return ()
        else intCodeComputer
             . writeback
             . execute
             . decode
             $ fetch currState

fetch :: CompState -> IO CompState
fetch state = do
    let opcode = tape state !! position state
    return state { opcode=opcode
          , op=mod opcode 100
          , modes=( mod (div opcode 100) 10
                  , mod (div opcode 1000) 10
                  , mod (div opcode 10000) 10
                  )
          }

decode :: IO CompState -> IO (CompState, (Int -> Int -> IO Int))
decode state = do
    state <- state
    return ( state { vals=getVals state, valP=getValP state }
               , getFunc state
               )

execute :: IO (CompState, Int -> Int -> IO Int) -> IO (CompState, IO Int)
execute arg = do
    (state, func) <- arg
    let (val1, val2, _) = vals state
    return (state, func val1 val2)

writeback :: IO (CompState, IO Int) -> IO CompState
writeback arg = do
    (state, valE) <- arg
    val <- valE
    return (updateState state val)

updateState :: CompState -> Int -> CompState
updateState state valE
    | op state < 3 =
        let (_, _, pos) = vals state
            (tapehd, tapetail) = splitAt pos $ tape state
        in  state { tape=tapehd ++ valE : tail tapetail, position=(position state) + (valP state) }
    | op state == inOp =
        let (pos, _, _) = vals state
            (tapehd, tapetail) = splitAt pos $ tape state
        in  state { tape=tapehd ++ valE : tail tapetail, position=(position state) + (valP state) }
    | op state == haltOp = state { halted=True, position=(position state) + (valP state) }
    | otherwise = state { position=(position state) + (valP state) }

getVals :: CompState -> (Int, Int, Int)
getVals state = let (mode1, mode2, _) = modes state
                in  ( getVal 1 mode1 state
                    , getVal 2 mode2 state
                    , getVal 3 1 state
                    )

getVal :: Int -> Int -> CompState -> Int
getVal pos mode state
    | op state == haltOp = 0
    | op state == inOp || op state == outOp && pos > 1 = 0
    | mode == 1 || op state == inOp = tape state !! (position state + pos)
    | otherwise = tape state !! (tape state !! (position state + pos))

solve ipt = do
    intCodeComputer (return (CompState { position=0
                                     , tape=ipt
                                     , input=getLine
                                     , output=putStrLn
                                     , halted=False
                                     , opcode=0
                                     , op=0
                                     , vals=(0, 0, 0)
                                     , modes=(0, 0, 0)
                                     , valP=0
                                     }))
    return ()

getValP :: CompState -> Int
getValP state
    | op state == inOp || op state == outOp = 2
    | op state == haltOp = 1
    | otherwise = 4

getFunc :: CompState -> (Int -> Int -> IO Int)
getFunc state
    | op state == 1 = (\x y -> return ((+) x y))
    | op state == 2 = (\x y -> return ((*) x y))
    | op state == 3 = getInput state
    | op state == 4 = writeOutput state
    | otherwise = (\x y -> return 0)

getInput :: CompState -> (Int -> Int -> IO Int)
getInput state =
    (\x y -> do
        val <- input state
        return (read val::Int))

writeOutput :: CompState -> (Int -> Int -> IO Int)
writeOutput state =
    (\x y -> do
        output state (show x)
        return 0)

prep :: String -> [Int]
prep s = case dropWhile (==',') s of
              "" -> []
              s' -> (read w::Int) : prep s''
                    where (w, s'') =
                           break (==',') s'

main = do
    contents <- readFile "day-5/input.txt"
    solve $ prep contents
