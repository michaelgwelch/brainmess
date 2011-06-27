module Instruction where

data Instruction = MoveForward
                 | MoveBackward
                 | Increment
                 | Decrement
                 | Output
                 | Input
                 | TestAndJumpForward
                 | TestAndJumpBackward deriving(Eq, Show)

parseInstruction :: Char -> Instruction
parseInstruction '>' = MoveForward
parseInstruction '<' = MoveBackward
parseInstruction '+' = Increment
parseInstruction '-' = Decrement
parseInstruction '.' = Output
parseInstruction ',' = Input
parseInstruction '[' = TestAndJumpForward
parseInstruction ']' = TestAndJumpBackward
