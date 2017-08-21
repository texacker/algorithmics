-- Computes the minimum depth of a binary tree

module Main where

main :: IO ()
main = print (show (min_depth testBTree))

testBTree :: BTree
testBTree = BTree_Bin (BTree_Bin (BTree_Leaf 1) (BTree_Bin (BTree_Leaf 2) (BTree_Leaf 3))) (BTree_Bin (BTree_Leaf 1) (BTree_Bin (BTree_Leaf 2) (BTree_Leaf 3)))

data BTree = BTree_Bin (BTree) (BTree)
           | BTree_Leaf (Int)
           deriving (Show)

type MD_BTree = Int -> Int -> ( Int )

min_depth :: BTree -> Int
min_depth bt = ( md bt ) 0 maxBound
  where
    md :: BTree -> MD_BTree
    md bt =
      case bt of
        (BTree_Bin _left _right) -> md_Bin (md _left) (md _right)
        (BTree_Leaf _value)      -> md_Leaf _value
      where
        md_Bin :: MD_BTree ->
                  MD_BTree ->
                  MD_BTree
        md_Bin left_ right_ =
          \ _depth
            _mindepth ->
            (
              let
                _left_depth :: Int
                _left_mindepth :: Int
                _left__mindepth :: Int
                _right_depth :: Int
                _right_mindepth :: Int
                _right__mindepth :: Int
                __mindepth :: Int

                _left_depth = ( _depth + 1 )
                _left_mindepth = _mindepth
                _left__mindepth = ( if _left_mindepth <= _left_depth then _left_mindepth else left_ _left_depth _left_mindepth )

                _right_depth = ( _depth + 1 )
                _right_mindepth = _left__mindepth
                _right__mindepth = ( if _right_mindepth <= _right_depth then _right_mindepth else right_ _right_depth _right_mindepth )

                __mindepth = _right__mindepth
              in
                __mindepth
            )
        md_Leaf :: Int ->
                   MD_BTree
        md_Leaf value_ =
          \ _depth
            _mindepth ->
            (
              let
                __mindepth :: Int
                __mindepth = ( if _mindepth <= _depth then _mindepth else _depth )
              in
                __mindepth
            )
