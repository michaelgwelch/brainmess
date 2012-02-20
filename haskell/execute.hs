module Execute where

import Prog
import Tape
import Data.Char

execute :: Prog a => Char -> a -> Tape -> IO Char -> (Char -> IO ()) 
           -> IO (a, Tape)
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
execute '[' p t _  _= if ((get t) == 0) then return ((jumpF p), t) 
                  else return (p, t)
execute ']' p t _ _ = if ((get t) /= 0) then return ((jumpB p), t)
                  else return (p, t)
execute _ p t _ _ = return (p, t)

