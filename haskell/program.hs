
import Tape
import Data.Char
import System

-- The Program value constructor should be kept hidden
data Program = Program String Int

-- probably inefficient (rethink later)
-- I'm not sure if behind the scenes haskell isn't a little smart about strings
endOfProgram :: Program -> Bool
endOfProgram (Program cs pos) = pos == (length cs)

fetch :: Program -> (Program, Char)
fetch (Program cs pos) = ((Program cs (pos+1)), cs !! pos)

jumpForward :: Program -> Program
jumpForward p = jump 1 p 1

jumpBackward :: Program -> Program
jumpBackward p = jump 1 p (-1)


jump:: Int -> Program -> Int -> Program
jump 0 p i  = p
jump n (Program cs pos) i = jump n' (Program cs (pos+1)) i
               where current = cs !! pos
                     n' = if (current=='[') then n+i
                          else if (current == ']') then n-i
                          else n


execute :: Char -> Program -> Tape -> IO Char -> (Char -> IO ()) 
           -> IO (Program, Tape)
execute '>' p t _ _ = return (p, moveF t)
execute '<' p t _ _ = return (p, moveR t)
execute '+' p t _ _ = return (p, inc t)
execute '-' p t _ _ = return (p, dec t)
execute '.' p t _ o = do
                    o $ chr $ get t
                    return (p, t)
execute ',' p t i _ = do
                    c <- i 
                    let t' = set t $ ord c
                    return (p, t')
execute '[' p t _  _= if ((get t) == 0) then return ((jumpForward p), t) 
                  else return (p, t)
execute ']' p t _ _ = if ((get t) /= 0) then return ((jumpBackward p), t)
                  else return (p, t)
execute _ p t _ _ = return (p, t)

run :: Program -> Tape -> IO (Program, Tape)
run p t = do
            let (p',i) = fetch p
            (p'',t') <- execute i p' t getChar putChar
            if (endOfProgram p'') then return (p'',t') else (run p'' t')

main :: IO ()
main = do
            args <- getArgs
            prog <- readFile $ args !! 1
            run (Program prog 0) tape
            return ()
