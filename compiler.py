import subprocess
import sys
import re

prelude = """
import Data.Array.IO
import Control.Monad

data Instruction = Instruction (IOArray Word Word -> Word -> Word -> Word -> IO ()) Word Word Word
instance Show Instruction where
    -- Somehow show operation name
    show (Instruction f x y z) = (show x) ++ " " ++ (show y) ++ " " ++ (show z)

letOp :: IOArray Word Word -> Word -> Word -> Word -> IO ()
letOp memory locX y _ = do
    writeArray memory locX y
    return ()

set :: IOArray Word Word -> Word -> Word -> Word -> IO ()
set memory locX locY _ = do
    val <- readArray memory locY
    writeArray memory locX val
    return ()

add :: IOArray Word Word -> Word -> Word -> Word -> IO ()
add memory locX locY locZ = do
    x <- readArray memory locX
    y <- readArray memory locY
    writeArray memory locZ (x + y)
    return ()

equals :: IOArray Word Word -> Word -> Word -> Word -> IO ()
equals memory locX locY locZ = do
    x <- readArray memory locX
    y <- readArray memory locY
    writeArray memory locZ (if x == y then 1 else 0)
    return ()

out :: IOArray Word Word -> Word -> Word -> Word -> IO ()
out memory locX _ _ = do
    x <- readArray memory locX
    putStrLn $ show $ x

doInstruction :: IOArray Word Word -> Instruction -> IO ()
doInstruction memory (Instruction f x y z) = do
    f memory x y z
    return ()

currentOpLoc = 0

runProgram :: IOArray Word Word -> IO ()
runProgram memory = do
    currentOp <- readArray memory currentOpLoc
    doInstruction memory (program !! (fromIntegral currentOp))
    currentOp <- readArray memory currentOpLoc
    writeArray memory currentOpLoc (currentOp + 1)
    if fromIntegral (currentOp + 1) < length program then do
        runProgram memory -- Recurse
    else do
        return () -- End Program

main = do
    -- 256 memory addresses that each store Words defaulting to 0
    memory <- newArray (0, 255) 0 :: IO (IOArray Word Word)
    -- currentOpLoc is 0
    runProgram memory

program :: [Instruction]
program =
"""

"""
Example program:
[ Instruction add 10 10 17
, Instruction equals 20 20 16
, Instruction set 16 0 0 ]
"""

pattern = re.compile(r'((let)|(set)|(add)|(eq\?)|(out)) (\d+)\s*(\d+)?\s*(\d+)?\s*(#.*)?')

def parse_line(line):
    match = pattern.match(line)
    if not match:
        print('Invalid line: ' + line)
        sys.exit(1)
    if match.group(1) == 'let':
        return 'letOp ' + match.group(7) + ' ' + match.group(8) + ' 0'
    elif match.group(1) == 'set':
        return 'set ' + match.group(7) + ' ' + match.group(8) + ' 0'
    elif match.group(1) == 'add':
        return 'add ' + match.group(7) + ' ' + match.group(8) + ' ' + match.group(9)
    elif match.group(1) == 'eq?':
        return 'equals ' + match.group(7) + ' ' + match.group(8) + ' ' + match.group(9)
    elif match.group(1) == 'out':
        return 'out ' + match.group(7) + ' 0 0'
    else:
        print('Something has gone terribly wrong with the compiler. Get Max.')
        sys.exit(1)

with open(sys.argv[1], 'r') as f:
    code = prelude
    prefix = '    [ Instruction '
    for line in f:
        if line.strip() == '' or line.strip().startswith('#'):
            continue
        code += prefix + parse_line(line) + '\n'
        prefix = '    , Instruction '
    code += '    ]'

with open('program.hs', 'w') as f:
    f.write(code)

subprocess.call(['ghc', 'program.hs'])
# print('Your program now exists in this folder.')
