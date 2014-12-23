module Main where

import Data.Foldable (foldlM)
import qualified Data.Vector.Unboxed.Mutable as M

import System.Environment (getArgs)
import System.IO (stdin, putChar)

import Data.Word (Word8)
import Data.ByteString.Internal (w2c, c2w)


type Pointer = Int
type Cell = Word8
type Memory = M.IOVector Cell
type Machine = (Pointer, Memory)

data Instruction =  PIncr Int
                   |PDecr Int
                   |CIncr Word8
                   |CDecr Word8
                   |Print
                   |Read
                   |Loop [Instruction]
                   deriving (Eq, Show)

-- parse and compile a Brainfuck code to instructions.
compile :: String -> [Instruction]
compile str = fst $ compile' ([], str)
  where
    compile' :: ([Instruction], String) -> ([Instruction], String)
    compile' (acc, "") = (reverse acc, "")
    compile' (PIncr n:acc, '>':xs) = compile' (PIncr (n+1):acc, xs)
    compile' (PDecr n:acc, '<':xs) = compile' (PDecr (n+1):acc, xs)
    compile' (CIncr n:acc, '+':xs) = compile' (CIncr (n+1):acc, xs)
    compile' (CDecr n:acc, '-':xs) = compile' (CDecr (n+1):acc, xs)
    compile' (acc,'>':xs) = compile' (PIncr 1:acc, xs)
    compile' (acc,'<':xs) = compile' (PDecr 1:acc, xs)
    compile' (acc,'+':xs) = compile' (CIncr 1:acc, xs)
    compile' (acc,'-':xs) = compile' (CDecr 1:acc, xs)
    compile' (acc,'.':xs) = compile' (Print:acc, xs)
    compile' (acc,',':xs) = compile' (Read:acc, xs)
    compile' (acc,'[':xs) = compile' (Loop subinstructs:acc, xss)
      where (subinstructs, xss) = compile' ([], xs)
    compile' (acc,']':xs) = (reverse acc, xs)
    compile' (acc, _:xs) = compile' (acc, xs) -- ignore any other character.

-- execute an instruction on a machine with a given state.
execute :: Machine -> Instruction -> IO Machine
execute machine@(pointer, memory) instruct= case instruct of
  PIncr n -> return (pointer+n, memory)
  PDecr n -> return (pointer-n, memory)
  CIncr n -> do
               c <- M.read memory pointer
               M.write memory pointer (c+n)
               return machine
  CDecr n -> do
               c <- M.read memory pointer
               M.write memory pointer (c-n)
               return machine
  Print -> do
             M.read memory pointer >>= putChar . w2c
             return machine
  Read -> do
            chr <- getChar
            M.write memory pointer (c2w chr)
            return machine
  Loop subinstructs -> do {loop machine subinstructs;}
    where
      loop m@(ptr, mem) i = do
                              c <- M.read mem ptr
                              if (i==[]) || c == 0
                                then return m
                                else do {m' <- run m i; loop m' i;}

-- execute a sequence of instructions.
run :: Machine -> [Instruction] -> IO Machine
run = foldlM execute 

-- read the first argument as a code, compile and execute it
-- on a machine with an initial state.
main :: IO ()
main' = do
         initial <- M.replicate 30000 0
         fmap (!!0) getArgs >>= run (0, initial) . compile
         return ()

main = do -- or read the first argument as a filename of a source code.
          initial <- M.replicate 30000 0
          filename <- fmap (!!0) getArgs
          readFile filename >>= run (0, initial) . compile
          return ()
