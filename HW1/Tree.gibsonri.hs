-- Name: Richard (Rikki) Gibson
-- ONID ID: gibsonri

module Tree where

import Prelude

--
-- * Part 2: Binary trees
--

-- | Integer-labeled binary trees.
data Tree = Node Int Tree Tree   -- ^ Internal nodes
          | Leaf Int             -- ^ Leaf nodes
  deriving (Eq,Show)


-- | An example binary tree, which will be used in tests.
t1 :: Tree
t1 = Node 1
      (Node 2
        (Node 3
          (Leaf 4)
          (Leaf 5))
        (Leaf 6))
      (Node 7
        (Leaf 8)
        (Leaf 9))

-- | Another example binary tree, used in tests.
t2 :: Tree
t2 = Node 6
      (Node 2
        (Leaf 1)
        (Node 4
          (Leaf 3)
          (Leaf 5)))
      (Node 8
        (Leaf 7)
        (Leaf 9))


-- | The integer at the left-most node of a binary tree.
--
--   >>> leftmost (Leaf 3)
--   3
--
--   >>> leftmost (Node 5 (Leaf 6) (Leaf 7))
--   6
--
--   >>> leftmost t1
--   4
--
--   >>> leftmost t2
--   1
--
leftmost :: Tree -> Int
leftmost (Leaf i)     = i
leftmost (Node _ l _) = leftmost l


-- | The integer at the right-most node of a binary tree.
--
--   >>> rightmost (Leaf 3)
--   3
--
--   >>> rightmost (Node 5 (Leaf 6) (Leaf 7))
--   7
--
--   >>> rightmost t1
--   9
--
--   >>> rightmost t2
--   9
--
rightmost :: Tree -> Int
rightmost (Leaf i) = i
rightmost (Node _ _ r) = rightmost r

-- Accumulates a function over a tree.
-- Produces invalid results for things besides search.
treeAcc :: (Int -> Int -> Int) -> Int -> Tree -> Int
treeAcc f n (Leaf m) = f n m
treeAcc f n (Node m l r) = f (applier res l) (applier res r)
  where applier = treeAcc f
        res = f n m

-- | Get the maximum integer from a binary tree.
--
--   >>> maxInt (Leaf 3)
--   3
--
--   >>> maxInt (Node 5 (Leaf 4) (Leaf 2))
--   5
--
--   >>> maxInt (Node 5 (Leaf 7) (Leaf 2))
--   7
--
--   >>> maxInt t1
--   9
--
--   >>> maxInt t2
--   9
--
maxInt :: Tree -> Int
maxInt = treeAcc max 0

-- | Get the minimum integer from a binary tree.
--
--   >>> minInt (Leaf 3)
--   3
--
--   >>> minInt (Node 2 (Leaf 5) (Leaf 4))
--   2
--
--   >>> minInt (Node 5 (Leaf 4) (Leaf 7))
--   4
--
--   >>> minInt t1
--   1
--
--   >>> minInt t2
--   1
--
minInt :: Tree -> Int
minInt (Leaf n) = n
minInt (Node n l r) = treeAcc min n (Node n l r)

-- | Get the sum of the integers in a binary tree.
--
--   >>> sumInts (Leaf 3)
--   3
--
--   >>> sumInts (Node 2 (Leaf 5) (Leaf 4))
--   11
--
--   >>> sumInts t1
--   45
--
--   >>> sumInts (Node 10 t1 t2)
--   100
--
sumInts :: Tree -> Int
sumInts (Leaf m) = m
sumInts (Node m l r) = m + sumInts l + sumInts r

-- | The list of integers encountered by a pre-order traversal of the tree.
--
--   >>> preorder (Leaf 3)
--   [3]
--
--   >>> preorder (Node 5 (Leaf 6) (Leaf 7))
--   [5,6,7]
--
--   >>> preorder t1
--   [1,2,3,4,5,6,7,8,9]
--
--   >>> preorder t2
--   [6,2,1,4,3,5,8,7,9]
--
preorder :: Tree -> [Int]
preorder (Leaf m) = [m]
preorder (Node m l r) = [m] ++ preorder l ++ preorder r


-- | The list of integers encountered by an in-order traversal of the tree.
--
--   >>> inorder (Leaf 3)
--   [3]
--
--   >>> inorder (Node 5 (Leaf 6) (Leaf 7))
--   [6,5,7]
--
--   >>> inorder t1
--   [4,3,5,2,6,1,8,7,9]
--
--   >>> inorder t2
--   [1,2,3,4,5,6,7,8,9]
--
inorder :: Tree -> [Int]
inorder (Leaf m) = [m]
inorder (Node m l r) = inorder l ++ [m] ++ inorder r

-- | Check whether a binary tree is a binary search tree.
--
--   >>> isBST (Leaf 3)
--   True
--
--   >>> isBST (Node 5 (Leaf 6) (Leaf 7))
--   False
--
--   >>> isBST t1
--   False
--
--   >>> isBST t2
--   True
--
--   >>> isBST t3
--   False

-- It's necessary to handle a case like the following
--       5
--     /   \
--    3     7
--   / \   / \
--  2   6 4   8
-- The range of acceptable values for a node needs to be passed to each subtree.
t3 :: Tree
t3 = Node 5
      (Node 3
        (Leaf 2) (Leaf 6))
      (Node 7
        (Leaf 4) (Leaf 8))

isBST :: Tree -> Bool
isBST t = isBSTAcc t Nothing Nothing

nodeValue :: Tree -> Int
nodeValue (Leaf value) = value
nodeValue (Node value _ _) = value

isBSTAcc :: Tree -> Maybe Int -> Maybe Int -> Bool
isBSTAcc (Leaf _) _ _ = True
isBSTAcc (Node value left right) minim maxim =
  let
    leftValue = nodeValue left
    rightValue = nodeValue right
    inRange n = maybe True (< n) minim && maybe True (> n) maxim
  in leftValue < value && inRange leftValue &&
    rightValue > value && inRange rightValue &&
    isBSTAcc left minim (Just value) &&
    isBSTAcc right (Just value) maxim
-- For the left subtree, the center value is the new maximum.
-- For the right subtree, the center value is the new minimum.

-- | Check whether a number is contained in a binary search tree.
--   (You may assume that the given tree is a binary search tree.)
--
--   >>> inBST 2 (Node 5 (Leaf 2) (Leaf 7))
--   True
--
--   >>> inBST 3 (Node 5 (Leaf 2) (Leaf 7))
--   False
--
--   >>> inBST 4 t2
--   True
--
--   >>> inBST 10 t2
--   False
--
inBST :: Int -> Tree -> Bool
inBST n (Leaf m) = n == m
inBST n (Node m l r)
  | n == m = True
  | n < m = inBST n l
  | n > m = inBST n r
  | otherwise = False
