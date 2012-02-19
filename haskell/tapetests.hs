import Tape

assertEqual :: (Eq a, Show a)  => a -> a -> String -> IO ()
assertEqual x y s | x == y = return ()
                  | otherwise = error $ "Expected: " ++ (show x) ++ ", Actual: "
                        ++ (show y) ++ " - " ++ s


tape_moveForward_InDefault :: IO ()
tape_moveForward_InDefault = let expected = Tape (Iterate [0,0] 1)
                                 actual = moveF tape
                             in assertEqual expected actual "MF1"

tape_moveForward_InMiddle :: IO ()
tape_moveForward_InMiddle = assertEqual expected actual "MF2"
        where expected = Tape (Iterate [1,3,5,7] 3)
              begin = Tape (Iterate [1,3,5,7] 2)
              actual = moveF begin 

tape_moveForward_AtEnd :: IO()
tape_moveForward_AtEnd = assertEqual expected actual "MF3"
        where expected = Tape (Iterate [2,4,6,8,10,0] 5)
              begin = Tape (Iterate [2,4,6,8,10] 4)
              actual = moveF begin
                            

tape_moveBackward_InDefault :: IO ()
tape_moveBackward_InDefault = let expected = Tape (Iterate [0,0] 0)
                                  actual = moveR tape
                             in assertEqual expected actual "MR1"

tape_moveBackward_InMiddle :: IO ()
tape_moveBackward_InMiddle = assertEqual expected actual "MR2"
        where expected = Tape (Iterate [1,3,5,7] 2)
              begin = Tape (Iterate [1,3,5,7] 3)
              actual = moveR begin 

tape_moveBackward_AtEnd :: IO()
tape_moveBackward_AtEnd = assertEqual expected actual "MR3"
        where expected = Tape (Iterate [2,4,6,8,10] 3)
              begin = Tape (Iterate [2,4,6,8,10] 4)
              actual = moveR begin

tape_setCurrent :: IO()
tape_setCurrent = assertEqual expected actual "SC1"
        where begin = createTape [7, 9, 11, 13, 14] 2
              actual = set begin 8
              expected = createTape [7, 9, 8, 13, 14] 2

tape_getCurrent :: IO ()
tape_getCurrent = assertEqual expected actual "GC1"
        where begin = createTape [99, 87, 65, 43] 3
              actual = get begin
              expected = 43


tape_inc :: IO()
tape_inc = assertEqual expected actual "Inc"
        where begin = createTape [1,3,5,7] 2
              actual = inc begin
              expected = createTape [1,3,6,7] 2
         
tape_dec :: IO()
tape_dec = assertEqual expected actual "Dec"
        where begin = createTape [1,3,5,7] 2
              actual = dec begin
              expected = createTape [1,3,4,7] 2
         

main :: IO ()
main = do
    tape_moveForward_InDefault
    tape_moveForward_InMiddle
    tape_moveForward_AtEnd
    tape_moveBackward_AtEnd
    tape_moveBackward_InDefault
    tape_moveBackward_AtEnd
    tape_setCurrent
    tape_getCurrent
    tape_inc
    tape_dec
    return ()

