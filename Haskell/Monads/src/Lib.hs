module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data BT a = Node (BT a) a (BT a) | Leaf deriving(Show, Ord)

instance Eq (BT a) where
    (Node left1 _ right1) == (Node left2 _ right2) = left1 == left2 && right1 == right2
    Leaf == Leaf = True
    node1 == node2 = False

instance Functor BT where
    fmap f (Node left a right) = Node (fmap f left) (f a) (fmap f right)
    fmap f Leaf = Leaf

withoutLeftmost :: BT a -> BT a
withoutLeftmost Leaf = error "cant do that"
withoutLeftmost (Node Leaf _ _) = Leaf
withoutLeftmost (Node left a right) = Node (withoutLeftmost left) a right

tree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

data Expr = Num Int
        --   | Lam String Expr
        --   | Var String
          | Plus Expr Expr
          | Times Expr Expr

eval :: Expr -> Int
eval (Plus left right) = eval left + eval right
eval (Times left right) = eval left * eval right
eval (Num n) = n


{--
     2
  1    3


--}




{--

interface BT {
    BT withoutLeftmost()
}

class Node implements BT {
    BT withoutLeftmost() {
        this.left.withoutLeftmostHelp(this)
    }

    BT withoutLeftmostHelp(this) {
        List<Integer> nums = ...
    }
}

class Leaf implements BT {
    BT withoutLeftmost() {
        error
    }
}

--}

