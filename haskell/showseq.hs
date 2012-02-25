module ShowSeq where

import Data.Sequence as Seq
import Data.Foldable as F(foldr)


-- | Pad the left side of the sequence with the specified
--   number of values.
padl :: Int -> a -> Seq a -> Seq a
padl 0 _ xs = xs
padl n a xs = padl (n-1) a (a <| xs)

-- | Pad the right side of the sequence with the specified
--   number of values.
padr :: Int -> a -> Seq a -> Seq a
padr 0 _ xs = xs
padr n a xs = padr (n-1) a (xs |> a)

-- | Truncates on the left to n elements
truncl :: Int -> Seq a -> Seq a
truncl n xs = Seq.drop (Seq.length xs - n) xs

-- | Truncates on the right to n elements
truncr n xs = Seq.take n xs

-- | Splits the sequence into a triple. 
splitAt3 :: Seq a -> Int -> (Seq a, a, Seq a)
splitAt3 xs n = (Seq.take n xs, index xs n, Seq.drop (n+1) xs)

-- | Shows the sequence as a tape
showSeqAsInf :: Seq Int -> Int -> String
showSeqAsInf xs i = "..., " ++ showSeq left ++ "_" ++ show mid ++
    "_, " ++ showSeq right ++ "..."
    where (l,mid,r) = splitAt3 xs i
          left = (truncl 5 (padl 5 0 l))
          right = (truncr 5 (padr 5 0 r))

showSeq :: Show a => Seq a -> String
showSeq xs = F.foldr (\x -> \cs -> show x ++ ", " ++ cs) "" xs



