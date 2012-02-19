module Program(createProgram, endOfProgram, fetch, run) where
import Tape
import Data.Char
import Data.Sequence
import Prelude hiding (length, take, drop)

-- The Program value constructor should be kept hidden
data Program = Program (Seq Char) Int deriving (Show, Eq)

createProgram :: String -> Program
createProgram s = Program (fromList s) 0

endOfProgram :: Program -> Bool
endOfProgram (Program cs pos) = pos == (length cs)

fetch :: Program -> (Program, Char)
fetch (Program cs pos) = ((Program cs (pos+1)), index cs pos)

jumpForward :: Program -> Program
jumpForward (Program s p) = (Program s ((match s (p-1))+1))

jumpBackward :: Program -> Program
jumpBackward (Program s p) = (Program s (match s (p-1)))

-- Finds the bracket that matches the one at index p of cs
match :: Seq Char -> Int -> Int
match cs p | (index cs p) == '[' = matchForward (viewl (drop (p+1) cs)) (p+1) 1 
           | (index cs p) == ']' = matchBackward (viewr (take p cs)) (p-1) 1 
           | otherwise = error "match: Current char is not '[' or ']'"

-- Takes a ViewL Char to search, the current position in overall sequence,
-- and the current nest level and returns the index of matching bracket.
matchForward :: ViewL Char -> Int -> Int -> Int
matchForward _ p 0 = (p-1)
matchForward ('[' :< cs) p n = matchForward (viewl cs) (p+1) (n+1)
matchForward (']' :< cs) p n = matchForward (viewl cs) (p+1) (n-1)
matchForward (_ :< cs) p n = matchForward (viewl cs) (p+1) n

matchBackward :: ViewR Char -> Int -> Int -> Int
matchBackward _ p 0 = (p+1)
matchBackward (cs :> '[') p n = matchBackward (viewr cs) (p-1) (n-1)
matchBackward (cs :> ']') p n = matchBackward (viewr cs) (p-1) (n+1)
matchBackward (cs :> _) p n = matchBackward (viewr cs) (p-1) n

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
run p t | (endOfProgram p) = return (p, t)
run p t = do
            let (p',i) = fetch p
            (p'',t') <- execute i p' t getChar putChar
            run p'' t'

