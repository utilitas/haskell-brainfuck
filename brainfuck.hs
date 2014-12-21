module Main where
import Data.Foldable (foldlM)
import Data.Sequence (Seq, empty, (|>))
import qualified Data.Map.Strict as M

import System.Environment (getArgs)
import System.IO (stdin, putChar)

import Data.Word (Word8)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS (w2c)


type Pointer = Int
type Cell = Word8
type Memory = M.Map Pointer Cell
type Machine = (Pointer, Memory)

data Instruction =  PIncr
                   |PDecr
                   |CIncr
                   |CDecr
                   |Print
                   |Read
                   |Loop (Seq Instruction)
                   deriving (Eq, Show)

-- parse and compile a Brainfuck code to instructions.
compile :: String -> Seq Instruction  
compile str = fst $ compile' (empty, str)
  where
    compile' :: (Seq Instruction, String) -> (Seq Instruction, String)
    compile' (acc, "") = (acc, "")
    compile' (acc,'>':xs) = compile' (acc |> PIncr, xs)
    compile' (acc,'<':xs) = compile' (acc |> PDecr, xs)
    compile' (acc,'+':xs) = compile' (acc |> CIncr, xs)
    compile' (acc,'-':xs) = compile' (acc |> CDecr, xs)
    compile' (acc,'.':xs) = compile' (acc |> Print, xs)
    compile' (acc,',':xs) = compile' (acc |> Read, xs)
    compile' (acc,'[':xs) = compile' (acc |> Loop subinstructs, xss)
      where (subinstructs, xss) = compile' (empty, xs)
    compile' (acc,']':xs) = (acc, xs)

-- execute an instruction on a machine with a given state. 
execute :: Machine -> Instruction -> IO Machine
execute machine@(pointer, memory) instruct = case instruct of
  PIncr -> return (pointer+1, memory)
  PDecr -> return (pointer-1, memory)
  CIncr -> return (pointer, incr pointer memory)
    where incr ptr mem = M.insert ptr (M.findWithDefault 0 ptr mem +1) mem
  CDecr -> return (pointer, decr pointer memory)
    where decr ptr mem = M.insert ptr (M.findWithDefault 0 ptr mem -1) mem
  Print -> do
            putChar $ BS.w2c $ M.findWithDefault 0 pointer memory
            return machine
  Read -> do
            byte <- BS.hGet stdin 1
            return (pointer, M.insert pointer (BS.head byte) memory)
  Loop subinstructs -> do {loop machine subinstructs;}
              where loop m@(ptr, mem) i = do
                      if (i==empty) || (M.findWithDefault 0 ptr mem) == 0
                        then return m
                        else do {m' <- run m i; loop m' i;}

-- execute a sequence of instructions.
run :: Machine -> Seq Instruction -> IO Machine 
run machine = foldlM execute machine

-- read the first argument as a code, compile and execute it
-- on a machine with an initial state.
main :: IO () 
main = do
         args <- getArgs
         run (0, M.empty) $ compile (args !! 0)
         return ()
         
