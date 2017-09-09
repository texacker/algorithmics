-- Refactoring of King and Launchbury's implementation of depth-first search

-- References :
-- [1] Fethi Rabhi, Guy Lapalme, Algorithms: A Functional Programming Approach, Addison-Wesley, 2nd ed., 1999.
-- [2] David King, John Launchbury, Structuring Depth-First Search Algorithms in Haskell, 1995.

module Main where

import Data.Ix

{--- List implementation ---}
newtype Set a = St [a]

emptySet :: Set a
setEmpty :: Set a -> Bool

emptySet = St []

setEmpty (St []) = True
setEmpty _       = False

{-  ordered without duplicates -}
inSet  :: (Ord a) => a -> Set a -> Bool
addSet :: (Ord a) => a -> Set a -> Set a
delSet :: (Ord a) => a -> Set a -> Set a


inSet x (St s) = elem x (takeWhile (<= x) s)

addSet x (St s) =
  St (add x s)
    where
      add x []                   = [x]
      add x s@(y:ys)| (x>y)      = y : (add x ys)
                    | (x<y)      = x : s
                    | otherwise  = s

delSet x (St s) =
  St (del x s)
    where
      del x []                   = []
      del x s@(y:ys)| (x>y)      = y : (del x ys)
                    | (x<y)      = s
                    | otherwise  = ys

{- end of ordered without duplicates -}

{--- end of List implementation ---}

{-- Pointer representation --}
data Graph v w = Graph v [ ( Graph v w, w ) ]

-- Figure 7.1 (c) in [1]
testGraph = v1
  where
    v1 = Graph 1 [(v2, 12), (v3, 34), (v5, 78)]
    v2 = Graph 2 [(v1, 12), (v4, 55), (v5, 32)]
    v3 = Graph 3 [(v1, 34), (v4, 61), (v5, 44)]
    v4 = Graph 4 [(v2, 55), (v3, 61), (v5, 93)]
    v5 = Graph 5 [(v1, 78), (v2, 32), (v3, 44), (v4, 93)]

{-- End of Pointer representation --}

data Tree v = Tree v [Tree v]
              deriving Show

type T_Graph_v v = Set v -> ( Set v, Maybe ( Tree v ) )

kldfs_g :: Ix v => Graph v w -> T_Graph_v v
kldfs_g (Graph v e) =
  \ m ->
    (
      if inSet v m then
        (m, Nothing)
      else
        let
          (m', ts) = kldfs_e (map fst e) (addSet v m)
        in
          (m', Just (Tree v ts))
    )

type T_Graph_e v = Set v -> ( Set v, [ Tree v ] )

kldfs_e :: Ix v => [ Graph v w ] -> T_Graph_e v
kldfs_e = foldr kldfs_e_cons kldfs_e_nil

kldfs_e_nil :: T_Graph_e v
kldfs_e_nil = \ m -> (m, [])

kldfs_e_cons :: Ix v => Graph v w -> T_Graph_e v -> T_Graph_e v
kldfs_e_cons g es =
  \ m ->
    (
      let
        ( m' , t' ) = kldfs_g g m
        ( m'', ts ) = es m'
        t'' =
          case t' of
            Nothing -> ts
            Just t  -> t:ts
      in
        (m'', t'')
    )

main :: IO ()
main = print (show (snd ( kldfs_g testGraph emptySet )))

--Output: Just (Tree 1 [Tree 2 [Tree 4 [Tree 3 [Tree 5 []]]]])
