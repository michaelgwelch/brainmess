
import Program 

assertEqual :: (Eq a, Show a)  => a -> a -> String -> IO ()
assertEqual x y s | x == y = return ()
                  | otherwise = error $ "Expected: " ++ (show x) ++ ", Actual: "
                        ++ (show y) ++ " - " ++ s

jump_BaseCase :: IO ()
jump_BaseCase = assertEqual expected actual "JB1"
        where begin = Program "[]" 2
              actual = jump 0 begin 1
              expected = begin

jump_BaseCase_Level1 :: IO()
jump_BaseCase_Level1 = assertEqual expected actual "JB2"
        where begin = Program "[]" 1
              actual = jump 1 begin 1
              expected = Program "[]" 2


jumpForward_Simple :: IO()
jumpForward_Simple = assertEqual expected actual "JF1"
        where begin = Program "[]" 1
              actual = jumpForward begin
              expected = Program "[]" 2

jumpForward_Simple2 :: IO()
jumpForward_Simple2 = assertEqual expected actual "JF2"
        where begin = Program "[    ]" 1
              actual = jumpForward begin
              expected = Program "[    ]" 6

jumpForward_Simple3 :: IO()
jumpForward_Simple3 = assertEqual expected actual "JF3"
        where begin = Program "[ [ ]]" 1
              actual = jumpForward begin
              expected = Program "[ [ ]]" 6

fetchTest :: IO()
fetchTest = assertEqual (fst (fetch (Program "++" 1))) (Program "++" 2) "fetch"

main :: IO ()
main = do
        jump_BaseCase
	jump_BaseCase_Level1
        jumpForward_Simple
        jumpForward_Simple2
        jumpForward_Simple3
        fetchTest
        return ()
