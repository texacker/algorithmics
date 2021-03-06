-- References :
-- [1] Richard Bird, Pearls of Functional Algorithm Design, Ch.1
-- [2] https://github.com/liuxinyu95/AlgoXY/blob/algoxy/others/preface/src/MinFree.hs

module Main where

import qualified Data.List as DL

main :: IO ()
main = print (show (min_free testList))

type T_minfree = ( [Int], Int, Int )

min_free :: [Int] -> Maybe Int
min_free a
  = last ( iterate f x )
      where
        last :: [ T_minfree ] -> Maybe Int
        last b = case b of
                   []     -> Nothing
                   (x:xs) -> case x of
                               ( [], l, _ ) -> Just l
                               otherwise    -> last xs

        -- minfree anamorphism

        x :: T_minfree
        x = ( a, 0, (DL.length a - 1) )

        f :: T_minfree -> T_minfree
        f ( xs, l, u ) | xs == []                  = ( [], l, u )
                       | DL.length as == m - l + 1 = ( bs, (m + 1), u )
                       | otherwise                 = ( as, l, m )
          where
            m = (l + u) `div` 2
            (as, bs) = DL.partition (<= m) xs

testList :: [Int]
testList = [18, 4, 8, 9, 16, 1, 14, 7, 19, 3, 0, 5, 2, 11, 6]
