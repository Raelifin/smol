import subprocess
import sys
import re

debugImports = """
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
"""

prelude = """
import Data.Array.MArray
import Data.Array.IO
import Control.Monad
import Data.Char (chr)

data Instruction = Instruction (IOUArray Word Word -> Word -> Word -> Word -> IO ()) Word Word Word
instance Show Instruction where
    -- Somehow show operation name
    show (Instruction f x y z) = (show x) ++ " " ++ (show y) ++ " " ++ (show z)

letOp :: IOUArray Word Word -> Word -> Word -> Word -> IO ()
letOp memory locX y _ = do
    writeArray memory locX y
    return ()

set :: IOUArray Word Word -> Word -> Word -> Word -> IO ()
set memory locX locY _ = do
    val <- readArray memory locY
    writeArray memory locX val
    return ()

add :: IOUArray Word Word -> Word -> Word -> Word -> IO ()
add memory locX locY locZ = do
    x <- readArray memory locX
    y <- readArray memory locY
    writeArray memory locZ (x + y)
    return ()

maxi :: IOUArray Word Word -> Word -> Word -> Word -> IO ()
maxi memory locX locY locZ = do
    x <- readArray memory locX
    y <- readArray memory locY
    writeArray memory locZ (max x y)
    return ()

equals :: IOUArray Word Word -> Word -> Word -> Word -> IO ()
equals memory locX locY locZ = do
    x <- readArray memory locX
    y <- readArray memory locY
    writeArray memory locZ (if x == y then 1 else 0)
    return ()

out :: IOUArray Word Word -> Word -> Word -> Word -> IO ()
out memory locX _ _ = do
    x <- readArray memory locX
    putStrLn $ show x

outChar :: IOUArray Word Word -> Word -> Word -> Word -> IO ()
outChar memory locX _ _ = do
    x <- readArray memory locX
    putStr [chr $ fromEnum x]

doInstruction :: IOUArray Word Word -> Instruction -> IO ()
doInstruction memory (Instruction f x y z) = f memory x y z

currentOpLoc = 0

step :: IOUArray Word Word -> IO (IOUArray Word Word)
step memory = do
  currentOp <- readArray memory currentOpLoc
  if fromIntegral currentOp < length program then do
    doInstruction memory (program !! (fromIntegral currentOp))
    currentOp <- readArray memory currentOpLoc
    writeArray memory currentOpLoc (currentOp + 1)
    return memory
  else return memory

runProgram :: IOUArray Word Word -> IO ()
runProgram memory = do
    step memory
    currentOp <- readArray memory currentOpLoc
    if fromIntegral currentOp < length program then do
        runProgram memory -- Continue
    else do
        return () -- End Program

program :: [Instruction]
program =
"""

"""
Example program:
[ Instruction add 10 10 17
, Instruction equals 20 20 16
, Instruction set 16 0 0 ]
"""

standardMain = """
main = do
    -- 256 memory addresses that each store Words defaulting to 0
    memory <- newArray (0, 255) 0 :: IO (IOUArray Word Word)
    runProgram memory
"""

debugMain = """
window :: Display
window = InWindow "smol debug" (800, 600) (10, 10)

background :: Color
background = white

fps :: Int
fps = 30

render :: IOUArray Word Word -> IO Picture
render memory = do
  elems <- getElems memory
  let memPic = translate (-380) 280 $ scale 0.1 0.1 $ renderMemoryElems elems
  return memPic

renderMemoryElems :: [Word] -> Picture
renderMemoryElems elems = results where
  results = pictures offsetPics
  offsetPics = offset <$> zip pics [0..]
  pics = text <$> show <$> elems
  offset (pic, idx) = translate (xOffset idx) (yOffset idx) pic
  xOffset idx = fromIntegral $ 600 * (idx `div` 25)
  yOffset idx = fromIntegral $ (-180) * (idx `mod` 25)

handleKeys :: Event -> IOUArray Word Word -> IO (IOUArray Word Word)
handleKeys (EventKey (SpecialKey KeySpace) Down _ _) memory = step memory
handleKeys _ memory = return memory

update :: Float -> IOUArray Word Word -> IO (IOUArray Word Word)
update _delta memory = return memory

main = do
  -- | 256 memory addresses that each store Words defaulting to 0
  memory <- newArray (0, 255) 0 :: IO (IOUArray Word Word)
  playIO window background fps memory render handleKeys update
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

def get_code(lines, debug=False):
    code = debugImports + prelude if debug else prelude
    prefix = '    [ Instruction '
    for line in lines:
        code += prefix + parse_line(line) + '\n'
        prefix = '    , Instruction '
    code += '    ]'
    code += debugMain if debug else standardMain
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

code = get_code(lines, len(sys.argv) > 2 and sys.argv[2] == '--debug')

with open('program.hs', 'w') as f:
    f.write(code)

subprocess.call(['stack', 'ghc', 'program.hs'])
# print('Your program now exists in this folder.')
