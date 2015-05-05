module Main where

import Data.Word (Word8)
import System.Environment (getArgs)

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

-- translate instructions into a C code.
translate :: [Instruction] -> String
translate [] = ""
translate (PIncr 1:xs) = "p++;" ++ translate xs
translate (PDecr 1:xs) = "p--;" ++ translate xs
translate (CIncr 1:xs) = "(*p)++;" ++ translate xs
translate (CDecr 1:xs) = "(*p)--;" ++ translate xs
translate (PIncr n:xs) = "p=p+" ++ show n ++ ";" ++ translate xs
translate (PDecr n:xs) = "p=p-" ++ show n ++ ";" ++ translate xs
translate (CIncr n:xs) = "*p=*p+" ++ show n ++ ";" ++ translate xs
translate (CDecr n:xs) = "*p=*p-" ++ show n ++ ";" ++ translate xs
translate (Print:xs) = "putchar(*p);" ++ translate xs
translate (Read:xs) = "getchar(*p);" ++ translate xs
translate (Loop sub:xs) = "while(*p){" ++ translate sub ++ "}" ++ translate xs

translate' str = header ++ (translate$compile$str) ++ footer
  where
    header = "#include <stdio.h>\n\
             \int main(){\
             \unsigned char m[30000]={};\
             \unsigned char *p=m;"
    footer = "}\n"

-- read the first argument as a name of a source file.
main :: IO ()
main = do
         filename <- fmap (!!0) getArgs
         readFile filename >>= putStr.translate'
