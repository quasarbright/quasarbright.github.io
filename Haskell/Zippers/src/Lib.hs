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
    moveUp (ListZipper [] rs) = Nothing
    moveUp (ListZipper (l:ls) rs) = Just (ListZipper ls (l:rs))

    moveLeft _= Nothing
    moveRight _ = Nothing

    moveDown (ListZipper ls []) = Nothing
    moveDown (ListZipper ls (r:rs)) = Just (ListZipper (r:ls) rs)

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

data Action s a = MoveAction Movement | EditAction (a -> a) | InsertAction a | DeleteAction | StateChangeAction (s -> Maybe a -> s)

runAction :: Zipper z => Action s a -> (s, z a) -> (s, Maybe (z a))
runAction (MoveAction   mv) (s, z) = (s, runMovement mv z)
runAction (EditAction   f ) (s, z) = (s, Just $ edit f z)
runAction (InsertAction a ) (s, z) = (s, Just $ insert a z)
runAction DeleteAction      (s, z) = (s, delete z)
runAction (StateChangeAction f) (s, z) = (f s (get z), Just z)


runActions :: (Zipper z) => [Action s a] -> (s, z a) -> (s, Maybe (z a))
runActions (act:acts) sz = ans 
    where
        (newS, newZ_) = runAction act sz
        ans = case newZ_ of
                Nothing -> (newS, Nothing)
                Just newZ -> runActions acts (newS, newZ)
runActions [] (s, z) = (s, Just z)

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
traverseList ls = (makeListZipper ls, replicate (length ls) MoveDown)


flattenTree :: BT a -> [a]
flattenTree tree = as
    where
        (z, mvs) = traverseTree tree
        acts = MoveAction <$> mvs
        stateChange = StateChangeAction f  -- accumulates a list of as
            where
                f s = maybe s (:s)
        newActs = reverse $ foldr comb [] (reverse acts) -- adds stateChange after every MoveUp 
        comb act acts = case act of
                            MoveAction MoveUp -> stateChange:act:acts
                            _ -> act:acts
        (as, _) = runActions newActs ([], z)


flattenList :: [a] -> [a]
flattenList ls = as
    where
        (z, mvs) = traverseList ls
        acts = MoveAction <$> mvs
        stateChange = StateChangeAction f  -- accumulates a list of as
            where
                f s = maybe s (:s)
        newActs = foldr (\act acts -> stateChange:act:acts) [] acts
        (as, _) = runActions newActs ([], z)