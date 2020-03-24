module Lib where

import           Control.Monad
import           Data.Maybe
someFunc :: IO ()
someFunc = putStrLn "someFunc"

class Zipper z where
    -- | move to the left not changing depth
    moveLeft :: z a -> Maybe (z a)
    -- | move to the right not changing depth
    moveRight :: z a -> Maybe (z a)
    -- | move closer to the root
    moveUp :: z a -> Maybe (z a)
    -- | move deeper in away from the root
    moveDown :: z a -> Maybe (z a)
    -- | insert a value at the focused location
    insert :: a -> z a -> z a
    -- | delete a whole "branch"
    delete :: z a -> Maybe (z a)
    -- | edit the currently focused value
    edit :: (a -> a) -> z a -> z a
    -- | get the current focused value, if any
    get :: z a -> Maybe a

data BT a = Leaf | Node (BT a) a (BT a) deriving(Eq,Show)
data BTPath a =
    Top -- the focus is the root
    | RightPath -- the focus is on the right side
        (BT a) -- the left sibling of the focused node
        a -- the parent data
        (BTPath a) -- the parent path
    | LeftPath -- the focus is on the left side
        (BTPath a) -- the parent path
        a -- the parent data
        (BT a) -- the right sibling of the focused node
    deriving(Eq,Show)
data BTZipper a = BTZipper (BT a) (BTPath a) deriving(Eq, Show)

instance Zipper BTZipper where
    moveLeft (BTZipper l (RightPath r a p)) = Just $ BTZipper r (LeftPath p a l)
    moveLeft _ = Nothing

    moveRight (BTZipper r (LeftPath p a l)) = Just $ BTZipper l (RightPath r a p)
    moveRight _ = Nothing

    moveDown (BTZipper Leaf _) = Nothing
    moveDown (BTZipper (Node l a r) p) = Just $ BTZipper l (LeftPath p a r)

    moveUp (BTZipper _ Top) = Nothing
    moveUp (BTZipper r (RightPath l a p)) = Just $ BTZipper (Node l a r) p
    moveUp (BTZipper l (LeftPath p a r)) = Just $ BTZipper (Node l a r) p

    insert a (BTZipper t p) = BTZipper (Node t a Leaf) p

    delete (BTZipper _ Top) = Nothing
    delete (BTZipper _ p@LeftPath{}) = Just (BTZipper Leaf p)
    delete (BTZipper _ p@RightPath{}) = Just (BTZipper Leaf p)

    edit f (BTZipper (Node l a r) p) = BTZipper (Node l (f a) r) p
    edit f z@(BTZipper Leaf p) = z

    get (BTZipper Leaf _) = Nothing
    get (BTZipper (Node _ a _) _) = Just a 

data ListZipper a = ListZipper [a] [a] deriving(Eq, Show)

instance Zipper ListZipper where
    moveDown (ListZipper [] rs) = Nothing
    moveDown (ListZipper (l:ls) rs) = Just (ListZipper ls (l:rs))

    moveLeft _= Nothing
    moveRight _ = Nothing

    moveUp (ListZipper ls []) = Nothing
    moveUp (ListZipper ls (r:rs)) = Just (ListZipper (r:ls) rs)

    insert a (ListZipper ls rs) = ListZipper ls (a:rs)

    delete (ListZipper ls (r:rs)) = Just (ListZipper ls rs)
    delete (ListZipper (l:ls) []) = Just (ListZipper ls [])
    delete (ListZipper [] []) = Nothing

    edit f (ListZipper ls (r:rs)) = ListZipper ls (f r : rs)
    edit f (ListZipper (l:ls) []) = ListZipper (f l:ls) []
    edit f (ListZipper [] []) = ListZipper [] []

    get (ListZipper ls (r:rs)) = Just r 
    get (ListZipper (l:ls) []) = Just l
    get (ListZipper [] []) = Nothing

makeListZipper :: [a] -> ListZipper a
makeListZipper = ListZipper []

makeTreeZipper :: BT a -> BTZipper a
makeTreeZipper = flip BTZipper Top

data Movement = MoveLeft | MoveRight | MoveUp | MoveDown deriving(Eq, Show)

runMovement :: Zipper z => Movement -> z a -> Maybe (z a)
runMovement MoveLeft  z = moveLeft z
runMovement MoveRight z = moveRight z
runMovement MoveUp    z = moveUp z
runMovement MoveDown  z = moveDown z

runMovements :: (Zipper z, Foldable f) => f Movement -> z a -> Maybe (z a)
runMovements mvs z = foldM (flip runMovement) z mvs

data Action a = MoveAction Movement | EditAction (a -> a) | InsertAction a | DeleteAction

runAction :: Zipper z => Action a -> z a -> Maybe (z a)
runAction (MoveAction   mv) z = runMovement mv z
runAction (EditAction   f ) z = Just $ edit f z
runAction (InsertAction a ) z = Just $ insert a z
runAction DeleteAction      z = delete z

runActionAndCombine :: Zipper z => (a -> Action a -> b -> b) -> b -> Action a -> z a -> (b, Maybe (z a))
runActionAndCombine f acc act z = (fromMaybe acc newAcc_, newZ_)
  where
    newZ_ = runAction act z
    newAcc_ = do
            newZ <- newZ_
            a <- get newZ
            return $ f a act acc 

runActions :: (Zipper z, Foldable f) => f (Action a) -> z a -> Maybe (z a)
runActions acts z = foldM (flip runAction) z acts

runActionsAndCombine f acc [] z = acc
runActionsAndCombine f acc (act:acts) z = ans
    where
        (newAcc, newZ_) = runActionAndCombine f acc act z
        ans = case newZ_ of
                Nothing -> newAcc
                Just newZ -> runActionsAndCombine f newAcc acts newZ


-- | returns the starter zipper and the movements which will traverse it
-- | if you run an edit after any up movement, you'll map the tree
-- | basically goes down until it can't anymore then goes right, then up
-- | If the focus is on the left and you just went up, go right
-- | If the focus is on the right and you just went up, go up
traverseTree :: BT a -> (BTZipper a, [Movement])
traverseTree bt = (z, go bt)
  where
    z = makeTreeZipper bt
    go Leaf   = []
    go Node{} = loop z MoveDown

    loop (BTZipper Leaf   Top) _      = []
    loop (BTZipper Node{} Top) MoveUp = [] -- done
    loop zz                     mv     = newMv : loop newZ newMv
      where
        newMv = decide zz mv
        newZ  = fromMaybe (error "move failed in traversal") (runMovement newMv zz)
    --
    decide (BTZipper Node{} Top) MoveDown = MoveDown
    decide (BTZipper _ Top) _ = error "shouldn't decide top unless beginning"
    decide _ MoveLeft = error "should never have moved left"
    decide (BTZipper _ LeftPath{}) MoveRight =
        error "got to a left by going right?"
    decide (BTZipper Leaf LeftPath{} ) _      = MoveRight
    decide (BTZipper _    LeftPath{} ) MoveUp = MoveRight
    decide (BTZipper Leaf RightPath{}) _      = MoveUp
    decide (BTZipper _    RightPath{}) MoveUp = MoveUp
    decide z                           mv      = MoveDown

        -- decide (BTZipper _      LeftPath{} ) MoveUp    = MoveRight
        -- decide (BTZipper Leaf   LeftPath{} ) _         = MoveRight
        -- decide (BTZipper Node{} RightPath{}) MoveRight = MoveDown
        -- decide (BTZipper Node{} _          ) MoveDown  = MoveDown
        -- decide (BTZipper Leaf   RightPath{}) _         = MoveUp
        -- decide (BTZipper _      RightPath{}) MoveUp    = MoveUp

traverseList :: [a] -> (ListZipper a, [Movement])
traverseList ls = (makeListZipper ls, replicate (length ls) MoveUp)


flattenTree :: BT a -> [a]
flattenTree tree = flattenZipper z mvs
    where
        (z, mvs) = traverseTree tree


flattenList :: [a] -> [a]
flattenList ls = flattenZipper z mvs
    where
        (z, mvs) = traverseList ls

flattenZipper :: Zipper z => z a -> [Movement] -> [a]
flattenZipper z mvs = ans
    where
        acts = MoveAction <$> mvs
        combiner a (MoveAction MoveUp) acc = a:acc
        combiner _ _ acc = acc
        ans = runActionsAndCombine combiner [] acts z

lf = Leaf
t = Node (Node lf 'a' (Node lf 'b' lf)) 'c' (Node (Node lf 'd' lf) 'e' (Node lf 'f' lf))

-- left off debugging flattenList. bug is happening because due to the nature of runAndCombine, the combiner never sees the first element and seemingly sees the last twice?
-- a more robust way to do all of this would be to keep around a (zipper, state) pair and have a state manipulation action with a (a -> s -> s)
-- then you'd just selectively place that action in the traversal list according to what you're doing