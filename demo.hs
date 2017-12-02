import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game


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

program :: [Instruction]
program =
    [ Instruction letOp 10 0 0
    , Instruction letOp 11 1 0
    , Instruction letOp 12 2 0
    , Instruction letOp 13 3 0
    , Instruction letOp 14 4 0
    , Instruction letOp 15 5 0
    , Instruction letOp 16 6 0
    , Instruction letOp 17 7 0
    , Instruction letOp 18 8 0
    , Instruction letOp 19 9 0
    , Instruction set 101 0 0
    , Instruction add 0 12 0
    , Instruction add 11 1 3
    , Instruction set 0 4 0
    , Instruction letOp 5 42 0
    , Instruction letOp 32 32 0
    , Instruction letOp 99 8595 0
    , Instruction letOp 102 128513 0
    , Instruction letOp 103 10 0
    , Instruction out 5 0 0
    , Instruction outChar 32 0 0
    , Instruction outChar 99 0 0
    , Instruction outChar 103 0 0
    , Instruction set 1 5 0
    , Instruction add 0 11 4
    , Instruction add 11 101 0
    , Instruction out 3 0 0
    , Instruction outChar 102 0 0
    , Instruction outChar 103 0 0
    ]
