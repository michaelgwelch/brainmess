module Program(createProgram, endOfProgram, fetch, run) where
import Prog
import Tape
import Execute
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


run :: Program -> Tape -> IO (Program, Tape)
run p t | (endOfProgram p) = return (p, t)
run p t = do
            let (p',i) = fetch p
            (p'',t') <- execute i p' t getChar putChar
            run p'' t'


instance Prog Program where
    jumpF = jumpForward
    jumpB = jumpBackward
