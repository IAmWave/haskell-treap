module Treap where

import System.Random

type Weight = Int
type Size = Int

-- Elements with higher weights are higher.
data Treap v = Empty | Treap Size Weight v (Treap v) (Treap v)
    deriving (Eq)

instance Show a => Show (Treap a) where
    show = showTree
    -- show Empty = ""
    -- show (Treap w a ls rs) = (show a)++"|"++(show w)++"(" ++ (show ls) ++ ";" ++ (show rs) ++ ")"

empty :: Treap a
empty = Empty

singleton :: RandomGen g => g -> a -> (g, Treap a)
singleton g a = (g', Treap 1 w a Empty Empty)
    where (w, g') = next g

-------------- Lookup --------------
isEmpty :: Treap a -> Bool
isEmpty Empty = True
isEmpty t = False

size :: Treap a -> Size
size Empty = 0
size (Treap n _ _ _ _) = n

member :: Ord a => a -> Treap a -> Bool
member _ Empty = False
member a (Treap _ _ v ls rs)
    | a < v     = member a ls
    | a == v    = True
    | a > v     = member a rs

-- Return the i-th element (indexed from 0) of a treap, or throw an error
elemAt :: Ord a => Int -> Treap a -> a
elemAt _ Empty = error "Treap.elemAt: index out of range"
elemAt i (Treap n _ a ls rs)
    | i < (size ls)  = elemAt i ls
    | i == (size ls) = a
    | i > (size ls)      = elemAt (i-(size ls)-1) rs

-- Get the index of an element `a` (inverse of elemAt).
-- If `a` is present multiple times, findIndex may return any of the indices.
findIndex :: Ord a => a -> Treap a -> Int
findIndex = findIndex' 0
    where
        findIndex' :: Ord a => Int -> a -> Treap a -> Int
        findIndex' _ _ Empty = error "Treap.findIndex: Error: element is not in the set"
        findIndex' i a' (Treap n w a ls rs)
            | a' == a = (i + (size ls))
            | a' <  a = findIndex' i a' ls
            | a' >  a = findIndex' (i + 1 + (size ls)) a' rs

-------------- Modification --------------
insert :: (RandomGen g, Ord a) => g -> a -> Treap a -> (g, Treap a)
insert g a treap = (g', insertWithWeight w a treap)
    where (w, g') = next g

-- First insert at the bottom, then rotate to maintain the heap invariant
insertWithWeight :: Ord a => Weight -> a -> Treap a -> Treap a
insertWithWeight w a Empty = Treap 1 w a Empty Empty
insertWithWeight w a (Treap n w' a' ls' rs')
    | a <= a' = rotate $ Treap (n+1) w' a' (insertWithWeight w a ls') rs'
    | a >  a' = rotate $ Treap (n+1) w' a' ls' (insertWithWeight w a rs')

delete :: Ord a => a -> Treap a -> Treap a
delete a Empty = Empty      -- No error - mirrors behavior of Data.Set
delete a (Treap n' w' a' ls rs)
    | a < a'     = updateSize $ Treap n' w' a' (delete a ls) rs
    | a > a'     = updateSize $ Treap n' w' a' ls (delete a rs)
    | a == a'    = deleteRoot (Treap n' w' a' ls rs)

-- Delete the root element of the treap
deleteRoot :: Ord a => Treap a -> Treap a
deleteRoot Empty = error "Cannot delete root of an empty treap"
-- Trick: force rotations by setting w=-1
deleteRoot (Treap n w a ls rs)
    | ls == Empty && rs == Empty = Empty
    | ls == Empty || lw <= rw              = updateSize $ mapLeftSon deleteRoot $ rotateR (Treap n (-1) a ls rs)
    | rs == Empty || lw >  rw              = updateSize $ mapRightSon deleteRoot $ rotateL (Treap n (-1) a ls rs)
    | otherwise = error "This shouldn't happen!"
        where
            getWeight Empty = -1
            getWeight (Treap _ w' _ _ _) = w'
            lw = getWeight ls
            rw = getWeight rs

-------------- Upkeep --------------
rotate :: Ord a => Treap a -> Treap a
rotate = rotateL . rotateR

-- Maintain the heap invariant of the left son (meaning a *right* rotation is performed if necessary)
rotateL :: Ord a => Treap a -> Treap a
rotateL Empty = Empty
rotateL (Treap n w a Empty rs) = Treap n w a Empty rs
rotateL (Treap n w a (Treap ln lw la lls lrs) rs)
    | lw > w      = updateSize $ Treap ln lw la lls (updateSize $ Treap n w a lrs rs)
    | otherwise   = (Treap n w a (Treap ln lw la lls lrs) rs) -- no rotation

-- Maintain the heap invariant of the right son (meaning a *left* rotation is performed if necessary)
rotateR :: Ord a => Treap a -> Treap a
rotateR Empty = Empty
rotateR (Treap n w a ls Empty) = Treap n w a ls Empty
rotateR (Treap n w a ls (Treap rn rw ra rls rrs))
    | rw > w      = updateSize $ Treap rn rw ra (updateSize $ Treap n w a ls rls) rrs
    | otherwise   = Treap n w a ls (Treap rn rw ra rls rrs) -- no rotation

-- Recalculate size from the sons' sizes
updateSize :: Treap a -> Treap a
updateSize Empty = Empty
updateSize (Treap n w v ls rs) = (Treap ((size ls) + (size rs) + 1) w v ls rs)


-------------- List conversion --------------
fromList :: (RandomGen g, Ord a) => g -> [a] -> (g, Treap a)
fromList g [] = (g, Empty)
fromList g (a:as) = insert g' a rest
    where (g', rest) = fromList g as

toList :: Treap a -> [a]
toList l = toList' l []
    where
        toList' Empty acc = acc
        toList' (Treap _ _ a ls rs) acc0 = acc2
            where acc2 = toList' ls (a:acc1)
                  acc1 = toList' rs acc0


-------------- Debugging/utility --------------
showTree :: Show a => Treap a -> String
showTree t = showTree' t [] True

indent :: [Bool] -> String
indent [] = ""
indent (x:xs) = (foldl (++) "" $ reverse $ map (\x -> if x then "|  " else "   ") xs) ++ "+--"

showTree' :: Show a => Treap a -> [Bool] -> Bool -> String
showTree' Empty d _ = (indent d) ++ "ø\n"
showTree' (Treap _ w a ls rs) d lastRight = lPart ++ myLine ++ "\n" ++ rPart
    where
        myLine = (indent d) ++ (show a) ++ " (" ++ (show w) ++ ")"
        dl = if (null d) then [] else (lastRight:(tail d))
        dr = if (null d) then [] else ((not lastRight):(tail d))
        lPart = if (isEmpty ls) then "" else (showTree' ls (True:dl)) False
        rPart = if (isEmpty rs) then "" else (showTree' rs (True:dr)) True

leftSon :: Treap a -> Treap a
leftSon (Treap _ _ _ ls _) = ls

rightSon :: Treap a -> Treap a
rightSon (Treap _ _ _ _ rs) = rs

mapLeftSon :: (Treap a -> Treap a) -> Treap a -> Treap a
mapLeftSon f Empty = Empty
mapLeftSon f (Treap n w a ls rs) = Treap n w a (f ls) rs

mapRightSon :: (Treap a -> Treap a) -> Treap a -> Treap a
mapRightSon f Empty = Empty
mapRightSon f (Treap n w a ls rs) = Treap n w a ls (f rs)
