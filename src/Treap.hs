module Treap where

import System.Random

type Weight = Int
type Size = Int

-- Elements with higher weights are higher.
data Treap v = Empty | Treap Size Weight v (Treap v) (Treap v)
    deriving (Eq)
instance Show a => Show (Treap a) where show = showTree

-------------- Construction --------------
empty :: Treap a
empty = Empty

singleton :: RandomGen g => g -> a -> (g, Treap a)
singleton g a = (g', Treap 1 w a Empty Empty)
    where (w, g') = next g

-------------- Lookup --------------
null :: Treap a -> Bool
null Empty = True
null t = False

size :: Treap a -> Size
size Empty = 0
size (Treap n _ _ _ _) = n

member :: Ord a => a -> Treap a -> Bool
member _ Empty = False
member a (Treap _ _ v ls rs) = case compare a v of
    LT -> member a ls
    GT -> member a rs
    EQ -> True

-- Return the i-th element (indexed from 0), or throw an error if the index is invalid
elemAt :: Ord a => Int -> Treap a -> a
elemAt _ Empty = error "Treap.elemAt: index out of range"
elemAt i (Treap n _ a ls rs) = case compare i (size ls) of
    LT -> elemAt i ls
    GT -> elemAt (i - (size ls) - 1) rs
    EQ -> a

-- Get the index of an element `a` (inverse of elemAt).
-- If `a` is present multiple times, findIndex may return any of the indices.
findIndex :: Ord a => a -> Treap a -> Int
findIndex = findIndex' 0
    where
        findIndex' :: Ord a => Int -> a -> Treap a -> Int
        findIndex' _ _ Empty = error "Treap.findIndex: Error: element is not in the set"
        findIndex' i a' (Treap n w a ls rs) = case compare a' a of
            LT -> findIndex' i a' ls
            GT -> findIndex' (i + 1 + (size ls)) a' rs
            EQ -> (i + (size ls))

-------------- Modification --------------
insert :: (RandomGen g, Ord a) => g -> a -> Treap a -> (g, Treap a)
insert g x treap = (g', insertWithWeight v x treap)
    where (v, g') = next g

-- First insert at the bottom, then rotate to maintain the heap invariant
insertWithWeight :: Ord a => Weight -> a -> Treap a -> Treap a
insertWithWeight v x Empty = Treap 1 v x Empty Empty
insertWithWeight v x (Treap n w a ls rs)
    | x <= a    = rotate $ Treap (n+1) w a (insertWithWeight v x ls) rs
    | otherwise = rotate $ Treap (n+1) w a ls (insertWithWeight v x rs)

delete :: Ord a => a -> Treap a -> Treap a
delete x Empty = Empty      -- No error - mirrors behavior of Data.Set
delete x (Treap n w a ls rs) = case compare x a of
    LT -> updateSize $ Treap n w a (delete x ls) rs
    GT -> updateSize $ Treap n w a ls (delete x rs)
    EQ -> deleteRoot (Treap n w a ls rs)

-- Delete the root element of the treap
deleteRoot :: Ord a => Treap a -> Treap a
deleteRoot Empty = error "Cannot delete root of an empty treap"
-- Trick: force rotations by setting w=-1
deleteRoot (Treap n w a ls rs)
    | (Treap.null ls) && (Treap.null rs)   = Empty
    | (Treap.null ls) || lw <= rw          = updateSize $ mapLeftSon  deleteRoot $ rotateR (Treap n (-1) a ls rs)
    | (Treap.null rs) || lw >  rw          = updateSize $ mapRightSon deleteRoot $ rotateL (Treap n (-1) a ls rs)
    | otherwise = error "This shouldn't happen!"
        where
            getWeight Empty = -1
            getWeight (Treap _ w' _ _ _) = w'
            lw = getWeight ls
            rw = getWeight rs

-------------- List conversion --------------
fromList :: (RandomGen g, Ord a) => g -> [a] -> (g, Treap a)
fromList g l = foldl (\(g, t) a -> insert g a t) (g, Empty) l

toList :: Treap a -> [a]
toList l = toList' l []
    where
        toList' Empty acc = acc
        toList' (Treap _ _ a ls rs) acc0 = acc2
            where acc2 = toList' ls (a:acc1)
                  acc1 = toList' rs acc0

-------------- Internal --------------
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

-------------- Debugging/utility --------------
showTree :: Show a => Treap a -> String
showTree t = showTree' t [] True

indent :: [Bool] -> String
indent [] = ""
indent (x:xs) = (foldl (++) "" $ reverse $ map (\x -> if x then "|  " else "   ") xs) ++ "+--"

showTree' :: Show a => Treap a -> [Bool] -> Bool -> String
showTree' Empty _ _ = "ø" -- Only happens with an empty treap
showTree' (Treap _ w a ls rs) d lastRight = lPart ++ myLine ++ "\n" ++ rPart
    where
        myLine = (indent d) ++ (show a) ++ " (" ++ (show w) ++ ")"
        dl = if (Prelude.null d) then [] else (lastRight:(tail d))
        dr = if (Prelude.null d) then [] else ((not lastRight):(tail d))
        lPart = if (Treap.null ls) then "" else (showTree' ls (True:dl)) False
        rPart = if (Treap.null rs) then "" else (showTree' rs (True:dr)) True

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
