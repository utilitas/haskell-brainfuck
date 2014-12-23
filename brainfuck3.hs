module Main where
import Data.Word (Word8)
import Data.ByteString.Internal (w2c, c2w)
import System.Environment (getArgs)

type Machine = ([Word8], Word8, [Word8])

data Instruction =  PIncr
                   |PDecr
                   |CIncr
                   |CDecr
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
    compile' (acc,'>':xs) = compile' (PIncr:acc, xs)
    compile' (acc,'<':xs) = compile' (PDecr:acc, xs)
    compile' (acc,'+':xs) = compile' (CIncr:acc, xs)
    compile' (acc,'-':xs) = compile' (CDecr:acc, xs)
    compile' (acc,'.':xs) = compile' (Print:acc, xs)
    compile' (acc,',':xs) = compile' (Read:acc, xs)
    compile' (acc,'[':xs) = compile' (Loop subinstructs:acc, xss)
      where (subinstructs, xss) = compile' ([], xs)
    compile' (acc,']':xs) = (reverse acc, xs)
    compile' (acc, _:xs) = compile' (acc, xs) -- ignore any other character.

run :: [Instruction] -> IO Machine
run instructions = run' instructions (repeat 0, 0, repeat 0)
  where run' [] m = return m
        run' i@(x:xs) m@(left@(l:ls),center,right@(r:rs)) = case x of
          PIncr -> run' xs (center:left, r, rs)
          PDecr -> run' xs (ls, l, center:right)
          CIncr -> run' xs (left, center+1, right)
          CDecr -> run' xs (left, center-1, right)
          Print -> (putChar.w2c) center >> run' xs m
          Read -> getChar >>= \char -> run' xs (left, c2w char, right)
          Loop sub -> if (center==0) || null sub
                        then run' xs m
                        else run' sub m >>= run' i

main = do
         filename <- fmap (!!0) getArgs
         readFile filename >>= run . compile
         return ()
