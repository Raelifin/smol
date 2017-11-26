import subprocess
import sys
import re

prelude = """
import Data.Array.IO
import Control.Monad
import Data.Char (chr)

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

maxi :: IOArray Word Word -> Word -> Word -> Word -> IO ()
maxi memory locX locY locZ = do
    x <- readArray memory locX
    y <- readArray memory locY
    writeArray memory locZ (max x y)
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
    putStrLn $ show x

outChar :: IOArray Word Word -> Word -> Word -> Word -> IO ()
outChar memory locX _ _ = do
    x <- readArray memory locX
    putStr [chr $ fromEnum x]

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

name_pattern = re.compile(r'([\w_][\w\d_]*)\s*=\s([\w\d\?]+)')
normal_pattern = re.compile(r'((let)|(set)|(add)|(max)|(eq\?)|(out)|(outChar)) (\d+)\s*(\d+)?\s*(\d+)?\s*(#.*)?')

def parse_line(line):
    match = normal_pattern.match(line)
    if not match:
        print('Invalid line: ' + line)
        sys.exit(1)
    try:
        if match.group(1) == 'let':
            return 'letOp ' + match.group(9) + ' ' + match.group(10) + ' 0'
        elif match.group(1) == 'set':
            return 'set ' + match.group(9) + ' ' + match.group(10) + ' 0'
        elif match.group(1) == 'add':
            return 'add ' + match.group(9) + ' ' + match.group(10) + ' ' + match.group(11)
        elif match.group(1) == 'max':
            return 'maxi ' + match.group(9) + ' ' + match.group(10) + ' ' + match.group(11)
        elif match.group(1) == 'eq?':
            return 'equals ' + match.group(9) + ' ' + match.group(10) + ' ' + match.group(11)
        elif match.group(1) == 'out':
            return 'out ' + match.group(9) + ' 0 0'
        elif match.group(1) == 'outChar':
            return 'outChar ' + match.group(9) + ' 0 0'
        else:
            print('Something has gone terribly wrong with the compiler. Get Max.')
            sys.exit(1)
    except TypeError as e:
        print('Mangled line: ' + line)
        print(match.groups())
        print(match.group(0))
        print(match.group(1))
        print(match.group(2))
        sys.exit(1)

def filter_out_comments_and_empty_lines(lines):
    return [ln for ln in lines if ln != '' and not ln.startswith('#')]

def substitute_names(lines):
    substitutions = []
    statements = []
    for line in lines:
        match = name_pattern.match(line)
        if match:
            substitutions.append((match.group(1), match.group(2)))
        else:
            statements.append(line)
    for name, value in substitutions:
        statements = [re.sub(r'\b' + name + r'\b', value, x) for x in statements]
    return statements

def get_code(lines):
    code = prelude
    prefix = '    [ Instruction '
    for line in lines:
        code += prefix + parse_line(line) + '\n'
        prefix = '    , Instruction '
    code += '    ]'
    return code

with open(sys.argv[1], 'r') as f:
    lines = []
    for line in f:
        lines.append(line.strip())

lines = filter_out_comments_and_empty_lines(lines)
lines = substitute_names(lines)

if len(lines) < 1:
    print("Your program must have at least one instruction.")
    sys.exit(1)

code = get_code(lines)

with open('program.hs', 'w') as f:
    f.write(code)

subprocess.call(['ghc', 'program.hs'])
# print('Your program now exists in this folder.')
