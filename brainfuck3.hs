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

run :: [Instruction] -> IO ()
run instructions = run' instructions (repeat 0, 0, repeat 0) >> return ()
  where
    run' :: [Instruction] -> Machine -> IO Machine
    run' [] m = return m
    run' (PIncr:xs) (ls, c, (r:rs)) = run' xs (c:ls, r, rs)
    run' (PDecr:xs) ((l:ls), c, rs) = run' xs (ls, l, c:rs)
    run' (CIncr:xs) (ls, c, rs) = run' xs (ls, c+1, rs)
    run' (CDecr:xs) (ls, c, rs) = run' xs (ls, c-1, rs)
    run' (Print:xs) (ls, c, rs) = (putChar.w2c) c >> run' xs (ls, c, rs)
    run' (Read:xs) (ls, c, rs) = getChar >>= \chr -> run' xs (ls, c2w chr, rs)
    run' (Loop sub:xs) (ls, 0, rs) = run' xs (ls, 0, rs)
    run' (Loop sub:xs) (ls, c, rs) = run' sub (ls, c, rs) >>= run' (Loop sub:xs)

main = do
         filename <- fmap (!!0) getArgs
         readFile filename >>= run . compile
