module Program where

import Instruction

-- The Program value constructor should be kept hidden
data Program = Program String Int

-- maybe inefficient (rethink later)
-- I'm not sure if behind the scenes haskell isn't a little smart about strings
endOfProgram :: Program -> Bool
endOfProgram (Program cs pos) = pos == (length cs)

fetch :: Program -> Instruction
fetch (Program cs pos) = parseInstruction $ cs !! pos

